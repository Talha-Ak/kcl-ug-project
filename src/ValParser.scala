package compiler
import fastparse._, ScalaWhitespace._
import compiler.ClosureConv.Env

object ValParser {

    sealed trait Exp
    sealed trait BExp
    trait Type

    case class Func(name: String, args: Seq[(String, Type)], ret: Type, body: Exp) extends Exp
    case class Const(i: String, t: Type, v: Exp) extends Exp
    case class Main(e: Exp) extends Exp

    case class Call(name: String, args: Seq[Exp]) extends Exp
    case class If(a: Exp, e1: Exp, e2: Exp) extends Exp // TODO: change to BExp
    case class Write(e: Exp) extends Exp
    case class Var(s: String) extends Exp
    case class Num(i: Int) extends Exp
    case class Op(a1: Exp, o: String, a2: Exp) extends Exp
    case class Sequence(e1: Exp, e2: Exp) extends Exp {
        override def toString = s"Sequence(\n$e1, $e2\n)"
    }

    case object Missing extends Type
    case class PrimType(name: String) extends Type
    case class FnType(args: Seq[Type], ret: Type) extends Type

    // // Identifiers
    // //===============
    def NumParser[$: P] =
        P(CharsWhileIn("0-9", 1)).!.map(_.toInt)

    def IdParser[$: P] =
        P(!StringIn("if", "then", "else", "write", "def") ~ CharIn("A-Za-z") ~~ CharsWhileIn("A-Za-z0-9_", 0)).!

    def TypeParser[$: P]: P[Type] =
        P("(" ~ TypeParser.rep(0, ",") ~ ")" ~ "=>" ~ TypeParser).map(FnType) |
        IdParser.map(PrimType) // TODO: Make bespoke parser for types
    
    def TypedIdParser[$: P] = P(IdParser ~ ":" ~ TypeParser)

    // Expressions
    //===============
    def Exp[$: P]: P[Exp] = P(
        ("if" ~ Equal ~ "then" ~ Exp ~ "else" ~ Exp).map(If).log("if") |
        ("write" ~ Equal).map(Write) |
        Block |
        Equal
    )

    def Block[$: P]: P[Exp] = P(
        ("{" ~/ (DefFn | DefVal).rep ~ Exp.rep(1, ";") ~ "}").map((a, b) => (a ++ b).reduceRight(Sequence)).log
    )

    def leftAssociate(a: Exp, b: Seq[(String, Exp)]): Exp = (a, b) match {
            case (`a`, (b, c) :: next) => leftAssociate(Op(a, b, c), next)
            case _ => a
    }
    def Equal[$: P]: P[Exp] = P(Comp ~ (StringIn("==", "!=").! ~ Comp).rep(0)).map(leftAssociate(_,_))
    def Comp[$: P]: P[Exp] = P(Term ~ (StringIn(">", ">=", "<", "<=").! ~ Term).rep(0)).map(leftAssociate(_,_))
    def Term[$: P]: P[Exp] = P(Fact ~ (CharIn("+\\-").! ~ Fact).rep(0)).map(leftAssociate(_,_)).log
    def Fact[$: P]: P[Exp] = P(Primary ~ (CharIn("/*%").! ~ Primary).rep(0)).map(leftAssociate(_,_))
    def Primary[$: P]: P[Exp] = P(
        (IdParser ~ "(" ~ Exp.rep(0, ",") ~ ")").map(Call) |
        ("(" ~ Exp ~ ")") |
        IdParser.map(Var) |
        NumParser.map(Num)
    )

    def DefFn[$: P]: P[Exp] =
        P("def" ~ IdParser ~ "(" ~ TypedIdParser.rep(0, ",") ~ ")" ~ ":" ~ TypeParser ~ "=" ~/ Exp ~ ";").map(Func).log
    def DefVal[$: P]: P[Exp] =
        P("val" ~ TypedIdParser ~ "=" ~/ Exp ~ ";").map(Const).log

    // def Prog[$: P]: P[List[Exp]] = P(
    //     ("def" ~ "main" ~ "(" ~ ")" ~ "=" ~/ Exp).map(s => List(Main(s))).log |
    //     (DefFn ~/ Prog).map((a, b) => a::b)
    // )

    def Prog[$: P]: P[Exp] = P(
        ("def" ~ "main" ~ "(" ~ ")" ~ "=" ~/ Exp).map(s => s).log |
        (DefFn ~/ Prog).map((a, b) => Sequence(a, b))
    )

    def All[$: P] = Prog ~ End
}
