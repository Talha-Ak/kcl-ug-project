package compiler
import ValParser._

// Functions containing pre-process steps before ANF conversion.
object PreProcess {

    // Removes Enum definitions from the AST and returns them seperately
    def extract_enums(e: Exp) : (Map[String, Type], Exp) = e match {
        case Sequence(e1: EnumDef, e2) => 
            val (enums, e_) = extract_enums(e2)
            (enums + (e1.name -> EnumType(e1.vals)), e_)
        case Sequence(e1, e2: EnumDef) => 
            val (enums, e_) = extract_enums(e1)
            (enums + (e2.name -> EnumType(e2.vals)), e_)
        case Sequence(e1, e2) => 
            val (enums1, e1_) = extract_enums(e1)
            val (enums2, e2_) = extract_enums(e2)
            (enums1 ++ enums2, Sequence(e1_, e2_))
        case e: EnumDef => (Map(e.name -> EnumType(e.vals)), e)
        case e => (Map(), e)
    }

    // Convert match statements to if-else statements
    def transform_match_to_if(e: Exp) : Exp = e match {
        case Match(a, cases) => 
            // The start value for the fold is a no-op in the case no default case is given
            cases.foldRight[Exp](Op(Num(0), "+", Num(0)))((mcase, acc) => {
                val MCase(root, item, exp) = mcase
                if root == "" && item == "_" then transform_match_to_if(exp)    // default
                else If(Op(Var(a), "==", EnumRef(root, item)), transform_match_to_if(exp), Some(acc))
            })
        case Func(name, args, ret, body) => Func(name, args, ret, transform_match_to_if(body))
        case Sequence(e1, e2) => Sequence(transform_match_to_if(e1), transform_match_to_if(e2))
        case If(a, e1, e2) => e2 match
            case Some(se2) => If(a, transform_match_to_if(e1), Some(transform_match_to_if(se2)))
            case None => If(a, transform_match_to_if(e1), None)
        case Write(e) => Write(transform_match_to_if(e))
        case Const(i, t, v) => Const(i, t, transform_match_to_if(v))

        case e => e
    }

    def preprocess(e: Exp) = extract_enums(transform_match_to_if(e))
}

// Functions containing post-process steps after ANF conversion.
object PostProcess {

    // Removes KConsts from the AST where unnecessary
    // E.g. LET a = 1
    //      CONST b = a
    def remove_extra_kconst(a: KLet, en: Map[String, Type]): KLet = a match {
        case KLet(x, e1, KConst(y, KVar(z, _), e2)) if x == z => KLet(y, process_kexp(e1, en), postprocess(e2, en))
        case KLet(x, v, e2) => KLet(x, process_kexp(v, en), postprocess(e2, en))
    }

    // Converts KConsts to KLets
    // E.g. CONST a = 1 -> LET a = 0 + 1
    //      CONST b = Enum::Sixth -> LET b = 0 + 5
    def convert_kconst_to_klet(a: KConst, en: Map[String, Type]): KAnf = {
        val KConst(x, v, e) = a
        v match
            case KVar(s, t) => throw new Exception("KConst with KVar should have been removed")
            case KNum(i) => KLet(x, Kop("+", KNum(0), KNum(i)), postprocess(e, en))
            case KBool(b) => KLet(x, Kop("+", KBool(false), KBool(b)), postprocess(e, en))
            case KFloat(f) => KLet(x, Kop("+", KFloat(0.0), KFloat(f)), postprocess(e, en))
            case v: KEnum =>
                en.get(v.root) match
                    case Some(EnumType(items)) =>
                        val idx = items.indexOf(v.item)
                        if idx == -1 then throw new Exception(s"Enum item ${v.item} not found in ${v.root}")
                        KLet(x, Kop("+", KNum(0), KNum(idx)), postprocess(e, en))
                    case Some(_) => throw new Exception(s"Enum ${v.root} not defined in ${en.keys}")
                    case None => throw new Exception(s"Enum root ${v.root} not found in ${en.keys}")
    }

    // Converts KVals where necessary
    // For now, only converts KEnum references to KNum
    def process_kval(a: KVal, en: Map[String, Type]): KVal = a match {
            case v: KEnum =>
                en.get(v.root) match
                    case Some(EnumType(items)) =>
                        val idx = items.indexOf(v.item)
                        if idx == -1 then throw new Exception(s"Enum ${v.item} not defined in ${v.root}")
                        KNum(idx)
                    case Some(_) => throw new Exception(s"Enum ${v.root} not defined in ${en.keys}")
                    case None => throw new Exception(s"Enum ${v.root} not defined in ${en.keys}")
            case v => v
    }

    // Processes KExp to check KVals
    def process_kexp(a: KExp, en: Map[String, Type]): KExp = a match {
        case KCall(v, vrs) => 
            val vs = vrs.map(v => process_kval(v, en))
            KCall(v, vs)
        case KStructDec(struct, vals) =>
            val vs = vals.map(v => process_kval(v, en))
            KStructDec(struct, vs)
        case Kop(o, v1, v2) => 
            Kop(o, process_kval(v1, en), process_kval(v2, en))
    }

    def postprocess(a: KAnf, enums: Map[String, Type]): KAnf = a match {
        case k: KLet => remove_extra_kconst(k, enums)
        case k: KConst => convert_kconst_to_klet(k, enums)
        case KIf(v, e1, e2) => KIf(v, postprocess(e1, enums), postprocess(e2, enums))
        case KFun(fnName, args, ret, body, in) => KFun(fnName, args, ret, postprocess(body, enums), postprocess(in, enums))
        case KWrite(v, in) => KWrite(v, postprocess(in, enums))
        case KReturn(v) => KReturn(v)
        case KLetEnv(x, env, next) => KLetEnv(x, env, postprocess(next, enums))
        case KLetEnvRef(x, ref, next) => KLetEnvRef(x, ref, postprocess(next, enums))
        case KStructDef(struct, next) => KStructDef(struct, postprocess(next, enums))
        case KStructRef(x, name, item, t, next) => KStructRef(x, name, item, t, postprocess(next, enums))
    }
    
}
