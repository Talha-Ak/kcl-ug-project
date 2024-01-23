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
    sealed trait KVal
    case class KVar(s: String) extends KVal
    case class KNum(i: Int) extends KVal

    sealed trait KExp
    case class Kop(o: String, v1: KVal, v2: KVal) extends KExp
    case class KCall(o: String, vrs: Seq[KVal]) extends KExp
    case class KExpVal(v: KVal) extends KExp

    trait KAnf
    case class KLet(x: String, e1: KExp, e2: KAnf) extends KAnf {
    override def toString = s"LET $x = $e1 in \n$e2" 
    }
    case class KIf(v: String, e1: KAnf, e2: KAnf) extends KAnf {
        def pad(e: KAnf) = e.toString.replaceAll("(?m)^", "  ")
        override def toString = s"IF $v\nTHEN\n${pad(e1)}\nELSE\n${pad(e2)}"
    }
    case class KFun(fnName: String, args: Seq[String], body: KAnf, in: KAnf) extends KAnf {
        override def toString = s"FUN $fnName($args): \n$body\n f-in \n$in"
    }
    case class KReturn(v: KVal) extends KAnf {
        override def toString = s"RETURN $v"
    }

    // CPS translation from Exps to KExps using a
    // continuation k.
    def CPS(e: Exp)(k: KVal => KAnf) : KAnf = e match {
        case Var(s) => k(KVar(s)) 
        case Num(i) => k(KNum(i))
        case Op(e1, o, e2) => {
            val z = Fresh("tmp")
            CPS(e1)(y1 => 
            CPS(e2)(y2 => KLet(z, Kop(o, y1, y2), k(KVar(z)))))
        }
        case If(Op(b1, o, b2), e1, e2) => {
            val z = Fresh("tmp")
            CPS(b1)(y1 => 
                CPS(b2)(y2 => 
                    KLet(z, Kop(o, y1, y2), KIf(z, CPS(e1)(k), CPS(e2)(k)))))
        }
        case Call(name, args) => {
            def aux(args: Seq[Exp], vs: List[KVal]) : KAnf = args match {
                case Nil => {
                    val z = Fresh("tmp")
                    KLet(z, KCall(name, vs), k(KVar(z)))
                }
                case e::es => CPS(e)(y => aux(es, vs ::: List(y)))
            }
            aux(args, Nil)
        }
        case Sequence(e1, e2) => 
            CPS(e1)(_ => CPS(e2)(y2 => k(y2)))
        // case Write(e) => {
        //     val z = Fresh("tmp")
        //     CPS(e)(y => KLet(z, KWrite(y), k(KVar(z))))
        // }
        case Const(i, t, v) =>
            CPS(v)(y => KLet(i, KExpVal(y), k(KVar(i))))
        case Func(name, args, ret, body) =>
            KFun(name, args.map(_._1).toList, CPS(body)(KReturn), k(KVar(name)))
        case Main(e) => CPS(e)(KReturn)
    }   

    //initial continuation
    def CPSi(e: Exp) = CPS(e)(KReturn)
}

