package compiler
import OriginalParser._

object OriginalCPS {

    // for generating new labels
    var counter = -1

    def Fresh(x: String) = {
        counter += 1
        x ++ "_" ++ counter.toString()
    }

    // Internal CPS language for FUN
    abstract class KExp
    abstract class KVal

    case class KVar(s: String) extends KVal
    case class KNum(i: Int) extends KVal
    case class Kop(o: String, v1: KVal, v2: KVal) extends KVal
    case class KCall(o: String, vrs: Seq[KVal]) extends KVal
    case class KWrite(v: KVal) extends KVal

    case class KLet(x: String, e1: KVal, e2: KExp) extends KExp {
    override def toString = s"LET $x = $e1 in \n$e2" 
    }
    case class KIf(x1: String, e1: KExp, e2: KExp) extends KExp {
    def pad(e: KExp) = e.toString.replaceAll("(?m)^", "  ")

    override def toString = 
        s"IF $x1\nTHEN\n${pad(e1)}\nELSE\n${pad(e2)}"
    }
    case class KReturn(v: KVal) extends KExp

    // CPS translation from Exps to KExps using a
    // continuation k.
    def CPS(e: Exp)(k: KVal => KExp) : KExp = e match {
        case Var(s) => k(KVar(s)) 
        case Num(i) => k(KNum(i))
        case Aop(o, e1, e2) => {
            val z = Fresh("tmp")
            CPS(e1)(y1 => 
            CPS(e2)(y2 => KLet(z, Kop(o, y1, y2), k(KVar(z)))))
        }
        case If(Bop(o, b1, b2), e1, e2) => {
            val z = Fresh("tmp")
            CPS(b1)(y1 => 
            CPS(b2)(y2 => 
                KLet(z, Kop(o, y1, y2), KIf(z, CPS(e1)(k), CPS(e2)(k)))))
        }
        case Call(name, args) => {
            def aux(args: Seq[Exp], vs: List[KVal]) : KExp = args match {
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
        case Write(e) => {
            val z = Fresh("tmp")
            CPS(e)(y => KLet(z, KWrite(y), k(KVar(z))))
        }
    }   

    //initial continuation
    def CPSi(e: Exp) = CPS(e)(KReturn)
}

