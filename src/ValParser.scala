package compiler
import fastparse._, ScalaWhitespace._

object ValParser {

    sealed trait Exp
    case class Func(name: String, args: Seq[(String, Type)], ret: Type, body: Exp) extends Exp
    case class Const(i: String, t: Type, v: Exp) extends Exp
    case class EnumDef(name: String, vals: Seq[String]) extends Exp
    case class StructDef(name: String, items: Seq[(String, Type)]) extends Exp
    case class Main(e: Exp) extends Exp

    sealed trait BExp
    case class Call(name: String, args: Seq[Exp]) extends Exp
    case class If(a: Exp, e1: Exp, e2: Option[Exp]) extends Exp // TODO: change to BExp
    case class Match(a: String, cases: Seq[MCase]) extends Exp
    case class MCase(root: String, item: String, exp: Exp)
    case class Write(e: Exp) extends Exp
    case class Var(s: String) extends Exp
    case class EnumRef(root: String, item: String) extends Exp
    case class StructRef(root: String, item: String) extends Exp
    case class Num(i: Int) extends Exp
    case class Bool(b: Boolean) extends Exp
    case class Flt(f: Double) extends Exp // LLVM requires double precision floats.
    case class Op(a1: Exp, o: String, a2: Exp) extends Exp
    case class Sequence(e1: Exp, e2: Exp) extends Exp {
        override def toString = s"Sequence(\n$e1, $e2\n)"
    }

    sealed trait Type
    case object Missing extends Type
    case object VoidType extends Type
    case object IntType extends Type
    case object BoolType extends Type
    case object FloatType extends Type
    case class UserType(name: String) extends Type
    case class FnType(args: Seq[Type], ret: Type) extends Type
    case class EnumType(items: Seq[String]) extends Type
    case class EnvType(env: String) extends Type

    // // Identifiers
    // //===============
    def NumParser[$: P] =
        P(CharsWhileIn("0-9", 1)).!.map(_.toInt)

    def BoolParser[$: P] =
        P("true").map(_ => true) |
        P("false").map(_ => false)

    def FloatParser[$: P] =
        P(CharsWhileIn("0-9", 1) ~ "." ~ CharsWhileIn("0-9", 1)).!.map(_.toDouble)

    def IdParser[$: P] =
        P(!StringIn("if", "then", "else", "print", "def", "val", "enum", "struct", "true", "false") ~ CharIn("A-Za-z_") ~~ CharsWhileIn("A-Za-z0-9_", 0)).!
        
    def EnumRefParser[$: P] =
        P(IdParser.! ~~ "::" ~~ IdParser.!)

    def StructRefParser[$: P] =
        P(IdParser.! ~~ "." ~~ IdParser.!)

    def TypeParser[$: P]: P[Type] =
        P("(" ~ TypeParser.rep(0, ",") ~ ")" ~ "=>" ~ TypeParser).map(FnType) |
        P("Int").map(_ => IntType) |
        P("Bool").map(_ => BoolType) |
        P("Void").map(_ => VoidType) |
        P("Float").map(_ => FloatType) |
        IdParser.map(UserType)
    
    def TypedIdParser[$: P] = P(IdParser ~ ":" ~ TypeParser)

    // Expressions
    //===============
    def Exp[$: P]: P[Exp] = P(
        ("if" ~/ Equal ~ "then" ~ Exp ~ ("else" ~ Exp).?).map(If)|
        ("print" ~ "(" ~ Exp ~ ")").map(Write)|
        Match |
        Block |
        Equal
    )

    def Block[$: P]: P[Exp] = P(
        ("{" ~/ (DefFn | DefVal).rep ~ Exp.rep(1, ";") ~ "}").map((a, b) => (a ++ b).reduceRight(Sequence))
    )

    def Match[$: P]: P[Exp] = P(
        (IdParser ~ "match" ~/ "{" ~ Case.rep(1) ~ "}").map(Match)
    )

    def Case[$: P]: P[MCase] = P(
        ("case" ~ EnumRefParser ~ "=>" ~ Exp).map(MCase) |
        ("case" ~ "_" ~ "=>" ~ Exp).map(MCase("", "_", _))
    )

    def leftAssociate(a: Exp, b: Seq[(String, Exp)]): Exp = (a, b) match {
        case (`a`, (b, c) :: next) => leftAssociate(Op(a, b, c), next)
        case _ => a
    }

    def Equal[$: P]: P[Exp] = P(Comp ~ (StringIn("==", "!=").! ~ Comp).rep(0)).map(leftAssociate(_,_))
    def Comp[$: P]: P[Exp] = P(Term ~ (StringIn(">", ">=", "<", "<=").! ~ Term).rep(0)).map(leftAssociate(_,_))
    def Term[$: P]: P[Exp] = P(Fact ~ (CharIn("+\\-").! ~ Fact).rep(0)).map(leftAssociate(_,_))
    def Fact[$: P]: P[Exp] = P(Primary ~ (CharIn("/*%").! ~ Primary).rep(0)).map(leftAssociate(_,_))
    def Primary[$: P]: P[Exp] = P(
        (IdParser ~ "(" ~ Exp.rep(0, ",") ~ ")").map(Call) |
        ("(" ~ Exp ~ ")") |
        EnumRefParser.map(EnumRef) |
        StructRefParser.map(StructRef) |
        FloatParser.map(Flt) |
        NumParser.map(Num) |
        BoolParser.map(Bool) |
        IdParser.map(Var)
    )

    def DefFn[$: P]: P[Exp] =
        P("def" ~/ IdParser ~ "(" ~ TypedIdParser.rep(0, ",") ~ ")" ~ ":" ~ TypeParser ~ "=" ~/ Exp ~/ ";").map(Func)
    def DefVal[$: P]: P[Exp] =
        P("val" ~/ TypedIdParser ~ "=" ~/ Exp ~/ ";").map(Const)
    def DefEnum[$: P]: P[Exp] =
        P("enum" ~/ IdParser ~ "=" ~ IdParser.rep(1, "|") ~/ ";").map(EnumDef)
    def DefStruct[$: P]: P[Exp] =
        P("struct" ~/ IdParser ~ "=" ~ "{" ~ TypedIdParser.rep(1, ",") ~ "}" ~/ ";").map(StructDef)

    def Prog[$: P]: P[Exp] = P(
        ("def" ~ "main" ~/ "(" ~/ ")" ~/ "=" ~/ Exp).map(s => s)|
        (DefFn ~/ Prog).map((a, b) => Sequence(a, b)) |
        (DefEnum ~/ Prog).map((a, b) => Sequence(a, b)) |
        (DefStruct ~/ Prog).map((a, b) => Sequence(a, b))
    )

    def All[$: P] = Prog ~ End
}
