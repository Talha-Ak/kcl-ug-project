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
    
    sealed trait KExp
    case class Kop(o: String, v1: KVal, v2: KVal) extends KExp
    case class KCall(v: KVar, vrs: Seq[KVal]) extends KExp
    case class KExpVal(v: KVal) extends KExp

    trait KAnf:
        def pad(e: KAnf) = e.toString.replaceAll("(?m)^", "  ")

    case class KLet(x: String, e1: KExp, e2: KAnf) extends KAnf:
        override def toString = s"LET $x = $e1 \n$e2" 

    case class KIf(v: String, e1: KAnf, e2: KAnf) extends KAnf:
        override def toString = s"IF $v \nTHEN \n${pad(e1)} \nELSE \n${pad(e2)}"

    case class KFun(fnName: String, args: Seq[(String, Type)], ret: Type, body: KAnf, in: KAnf) extends KAnf:
        override def toString = s"FUN $fnName($args): $ret = { \n${pad(body)} \n} \n\n$in"

    case class KWrite(v: KVal, in: KAnf) extends KAnf:
        override def toString = s"PRINT $v \n$in" 

    case class KReturn(v: KVal) extends KAnf:
        override def toString = s"RETURN $v"

    type TypeEnv = Map[String, Type]

    // CPS translation from Exps to KExps using a
    // continuation k.
    def CPS(e: Exp, ty: TypeEnv = Map())(k: (KVal, TypeEnv) => KAnf) : KAnf = e match {
        case Var(s) => k(KVar(s, ty(s)), ty) 
        case Num(i) => k(KNum(i), ty)
        case Bool(b) => k(KBool(b), ty)
        case Flt(f) => k(KFloat(f), ty)
        case Op(e1, o, e2) => {
            val z = Fresh("tmp")
            CPS(e1, ty)((y1, t1) => 
            CPS(e2, t1)((y2, t2) => KLet(z, Kop(o, y1, y2), k(KVar(z, y1.get_type), t2))))
        }
        case If(Op(b1, o, b2), e1, e2) => {
            val z = Fresh("tmp")
            CPS(b1, ty)((y1, t1) => 
                CPS(b2, t1)((y2, t2) => 
                    KLet(z, Kop(o, y1, y2), KIf(z, CPS(e1, t2)(k), CPS(e2, t2)(k))))) // TODO: Check if t2 for both is correct
        }
        case Call(name, args) => {
            def aux(args: Seq[Exp], vs: List[KVal], ty: TypeEnv) : KAnf = args match {
                case Nil => {
                    val z = Fresh("tmp")
                    val retTy = ty(name) match {
                        case FnType(_, t) => t
                        case _ => throw new Exception(s"Expected function type for $name")
                    }
                    val fn_call = KVar(name, retTy)
                    KLet(z, KCall(fn_call, vs), k(KVar(z, retTy), ty))
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
            CPS(v, updated_ty)((y, t1) => KLet(i, KExpVal(y), k(KVar(i), t1)))
        case Func(name, args, ret, body) =>
            val updated_ty = ty ++ args.map{case (x, t) => (x, t)} + (name -> FnType(args.map(_._2).toList, ret))
            KFun(name, args, ret, CPS(body, updated_ty)((y, _) => KReturn(y)), k(KVar(name), ty + (name -> FnType(args.map(_._2).toList, ret))))
        case Main(e) => CPS(e, ty)((y, _) => KReturn(y))
    }   

    //initial continuation
    def CPSi(e: Exp) = CPS(e)((y, _) => KReturn(y))
}
