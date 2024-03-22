package compiler
import ValParser._

object NewCPS {

    // for generating new labels
    var counter = -1

    def Fresh(x: String) = {
        counter += 1
        x ++ "_" ++ counter.toString()
    }

    // Internal CPS language
    sealed trait KVal:
        def get_type: Type

    case class KVar(s: String, t: Type = Missing) extends KVal:
        override def get_type = t

    case class KNum(i: Int) extends KVal:
        override def get_type = IntType

    case class KBool(b: Boolean) extends KVal:
        override def get_type = BoolType

    case class KFloat(f: Double) extends KVal:
        override def get_type = FloatType

    case class KEnum(root: String, item: String) extends KVal:
        override def get_type = IntType

    //case class KStructRef(name: String, item: String, t: Type = Missing) extends KVal:
    //   override def get_type = t
    
    sealed trait KExp
    case class Kop(o: String, v1: KVal, v2: KVal) extends KExp
    case class KStructDec(struct: String, vals: Seq[KVal]) extends KExp
    case class KCall(v: KVar, vrs: Seq[KVal]) extends KExp

    sealed trait KAnf:
        def pad(e: KAnf) = e.toString.replaceAll("(?m)^", "  ")

    case class KLet(x: String, e1: KExp, e2: KAnf) extends KAnf:
        override def toString = s"LET $x = $e1 \n$e2" 

    case class KConst(x: String, v: KVal, e: KAnf) extends KAnf:
        override def toString = s"CONST $x = $v \n$e"

    case class KIf(v: String, e1: KAnf, e2: KAnf) extends KAnf:
        override def toString = s"IF $v \nTHEN \n${pad(e1)} \nELSE \n${pad(e2)}"

    case class KFun(fnName: String, args: Seq[(String, Type)], ret: Type, body: KAnf, in: KAnf) extends KAnf:
        override def toString = s"FUN $fnName($args): $ret = { \n${pad(body)} \n} \n\n$in"

    case class KWrite(v: KVal, in: KAnf) extends KAnf:
        override def toString = s"PRINT $v \n$in" 

    case class KReturn(v: KVal) extends KAnf:
        override def toString = s"RETURN $v"

    // For handling closures
    case class Env(name: String, vals: Seq[KVal])
    case class Ref(env: KVar, idx: Int)
    case class KLetEnv(x: String, env: Env, next: KAnf) extends KAnf {
        override def toString = s"LET $x = $env \n$next"
    }
    case class KLetEnvRef(x: String, ref: Ref, next: KAnf) extends KAnf {
        override def toString = s"LET $x = $ref \n$next"
    }

    // For handling structs
    case class Struct(name: String, items: Seq[(String, Type)])
    case class KStructDef(struct: Struct, in: KAnf) extends KAnf:
        override def toString = s"STRUCT ${struct.name} = { \n${struct.items.map{case (x, t) => s"$x: $t"}.mkString("\n")} \n} \n$in"
    case class KStructRef(x: String, name: String, item: String, t: Type, next: KAnf) extends KAnf:
        override def toString = s"LET ${x}: $t =  ${name}.$item \n$next"

    type TypeEnv = Map[String, Type]

    // CPS translation from Exps to KExps using a
    // continuation k.
    def CPS(e: Exp, ty: TypeEnv = Map())(k: (KVal, TypeEnv) => KAnf) : KAnf = e match {
        case Var(s) => k(KVar(s, ty(s)), ty) 
        case Num(i) => k(KNum(i), ty)
        case Bool(b) => k(KBool(b), ty)
        case Flt(f) => k(KFloat(f), ty)
        case EnumRef(root, item) => k(KEnum(root, item), ty)
        case StructRef(name, item) =>
            ty(name) match {
                case UserType(t) =>
                    val z = Fresh("tmp")
                    // get the type of the item 
                    KStructRef(z, name, item, ty(name), k(KVar(z, ty(s"$t.$item")), ty))
                case _ => throw new Exception(s"Expected user type for $name, got ${ty(name)}")
            }
        case Op(e1, o, e2) => {
            val z = Fresh("tmp")
            CPS(e1, ty)((y1, t1) => 
            CPS(e2, t1)((y2, t2) => KLet(z, Kop(o, y1, y2), k(KVar(z, y1.get_type), t2))))
        }
        case If(Op(b1, o, b2), e1, e2) => {
            val z = Fresh("tmp")
            val next = e2.getOrElse(Op(Num(0), "+", Num(0)))
            CPS(b1, ty)((y1, t1) => 
                CPS(b2, t1)((y2, t2) => 
                    KLet(z, Kop(o, y1, y2), KIf(z, CPS(e1, t2)(k), CPS(next, t2)(k))))) // TODO: Check if t2 for both is correct
        }
        case Call(name, args) => {
            def aux(args: Seq[Exp], vs: List[KVal], ty: TypeEnv) : KAnf = args match {
                case Nil => {
                    val z = Fresh("tmp")
                    val fn_call = KVar(name, ty(name))
                    ty(name) match {
                        case t: FnType => KLet(z, KCall(KVar(name, t), vs), k(KVar(z, t.ret), ty))
                        case t: UserType => KLet(z, KStructDec(name, vs), k(KVar(z, ty(name)), ty))
                        case t => throw new Exception(s"Expected function type for $name, got ${ty(name)}")
                    }
                }
                case e::es => CPS(e, ty)((y, t1) => aux(es, vs ::: List(y), t1))
            }
            aux(args, Nil, ty)
        }
        case Sequence(e1, e2) => 
            CPS(e1, ty)((_, t1) => CPS(e2, t1)((y2, t2) => k(y2, t2)))
        case Write(e) =>
            CPS(e, ty)((y, t1) => KWrite(y, k(KVar("VOID", VoidType), t1)))
        case Const(i, t, v) =>
            val updated_ty = ty + (i -> t)
            CPS(v, updated_ty)((y, t1) => KConst(i, y, k(KVar(i), t1)))
        case Func(name, args, ret, body) =>
            val updated_ty = ty ++ args.map{case (x, t) => (x, t)} + (name -> FnType(args.map(_._2).toList, ret))
            KFun(name, args, ret, CPS(body, updated_ty)((y, _) => KReturn(y)), k(KVar(name), ty + (name -> FnType(args.map(_._2).toList, ret))))
        case StructDef(name, items) =>
            // update the ty with struct type, as well as items inside the struct
            val updated_ty = ty + (name -> UserType(name)) ++ items.map{case (x, t) => (s"$name.$x", t)}
            KStructDef(Struct(name, items), k(KVar(name), updated_ty))
        case Main(e) => CPS(e, ty)((y, _) => KReturn(y))
    }   

    //initial continuation
    def CPSi(e: Exp) = CPS(e)((y, ty) => {println(ty); KReturn(y)})
}
