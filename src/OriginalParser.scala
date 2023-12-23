package compiler
import fastparse._, ScalaWhitespace._

object OriginalParser {

    sealed trait Exp
    sealed trait BExp
    sealed trait Decl

    case class Def(name: String, args: Seq[String], body: Exp) extends Decl
    case class Main(e: Exp) extends Decl

    case class Call(name: String, args: Seq[Exp]) extends Exp
    case class If(a: BExp, e1: Exp, e2: Exp) extends Exp
    case class Write(e: Exp) extends Exp
    case class Var(s: String) extends Exp
    case class Num(i: Int) extends Exp
    case class Aop(o: String, a1: Exp, a2: Exp) extends Exp
    case class Sequence(e1: Exp, e2: Exp) extends Exp
    case class Bop(o: String, a1: Exp, a2: Exp) extends BExp

    // // Identifiers
    // //===============
    def NumParser[$: P] =
        P(CharsWhileIn("0-9", 1)).!.map(_.toInt)
    def IdParser[$: P] =
        P(!StringIn("if", "then", "else", "write", "def") ~ CharIn("A-Za-z") ~~ CharsWhileIn("A-Za-z0-9_", 0)).!

    // Expressions
    //===============
    def Exp[$: P]: P[Exp] = P(
        ("if" ~ BExp ~ "then" ~ Exp ~ "else" ~ Exp).map(If) |
        (M ~ ";" ~ Exp ~ !End).map(Sequence) |
        M
    )
    def M[$: P]: P[Exp] = P(
        ("write" ~ L).map(Write) |
        L
    )
    def L[$: P]: P[Exp] = P(
        (T ~ CharIn("+\\-").! ~ Exp).map((a1, s, a2) => Aop(s, a1, a2)) |
        T
    )
    def T[$: P]: P[Exp] = P(
        (F ~ CharIn("/*%").! ~ T).map((a1, s, a2) => Aop(s, a1, a2)) |
        F
    )
    def F[$: P]: P[Exp] = P(
        (IdParser ~ "(" ~ Exp.rep(0, ",") ~ ")").map(Call) |
        ("(" ~ Exp ~ ")") |
        IdParser.map(Var) |
        NumParser.map(Num)
    )

    def BExp[$: P]: P[BExp] = P(Exp ~ StringIn("==", "!=", "<", "<=", ">", ">=").! ~ Exp).map((a1, s, a2) => Bop(s, a1, a2))

    def Defn[$: P]: P[Decl] = P("def" ~ IdParser ~ "(" ~ IdParser.rep(0, ",") ~ ")" ~ "=" ~/ Exp).map(Def)

    def Prog[$: P]: P[List[Decl]] = P(
        (Defn ~ ";" ~/ Prog).map((a, b) => a::b) |
        Exp.map(s => List(Main(s)))
    )

    def All[$: P] = Prog ~ End

}
