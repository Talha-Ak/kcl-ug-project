package compiler
import fastparse._, ScalaWhitespace._

object Parser {

    sealed trait Expr
    sealed trait Decl
    sealed trait Stmt
    sealed trait Value

    case class Call(name: String, args: Seq[Expr]) extends Expr
    case class If(a: Expr, e1: Expr, e2: Option[Expr]) extends Expr
    case class Assign(l: Var, r: Expr) extends Decl
    case class Write(e: Expr) extends Decl
    
    case class Op(a1: Expr, o: String, a2: Expr) extends Expr
    case class UnaryOp(o: String, a: Expr) extends Expr
    
    case class Var(s: String) extends Expr
    case class Integer(s: Int) extends Expr
    case class Bool(b: Boolean) extends Expr
    case class Func(args: Seq[String], body: Expr) extends Expr


    def PLACEHOLDER[$: P] = P("a")

    def leftAssociate(a: Expr, b: Seq[(String, Expr)]): Expr = (a, b) match {
            case (`a`, (b, c) :: next) => leftAssociate(Op(a, b, c), next)
            case _ => a
    }

    // Identifiers
    //===============
    def NumIdent[$: P] =
        P(CharsWhileIn("0-9", 1)).!.map(_.toInt)
    def StrIdent[$: P] = P("\"" ~ AnyChar.rep(0) ~ "\"").!
    def VarIdent[$: P] =
        P(CharIn("A-Za-z") ~~ CharsWhileIn("A-Za-z0-9_", 0)).!

    // Expressions
    //===============
    def Expr[$: P]: P[Expr] = P(
        ("if" ~/ Equal ~ "then" ~/ Expr ~ ("else" ~/ Expr).?).map(If) |
        Equal
    )
    def Equal[$: P]: P[Expr] = P(Comp ~ (StringIn("==", "!=").! ~ Comp).rep(0)).map(leftAssociate(_,_))
    def Comp[$: P]: P[Expr] = P(Term ~ (StringIn(">", ">=", "<", "<=").! ~ Term).rep(0)).map(leftAssociate(_,_))
    def Term[$: P]: P[Expr] = P(Fact ~ (CharIn("+\\-").! ~ Fact).rep(0)).map(leftAssociate(_,_))
    def Fact[$: P]: P[Expr] = P(Unary ~ (CharIn("/*%").! ~ Unary).rep(0)).map(leftAssociate(_,_))
    def Unary[$: P]: P[Expr] = P((CharIn("!\\-").! ~ Unary).map(UnaryOp) | Primary)
    def Primary[$: P]: P[Expr] = P(
        "(" ~ Expr ~ ")" |
        (VarIdent ~ "(" ~ Expr.rep(0, ",") ~ ")").map(Call) | Value
    )
    def Value[$: P] = P(
        NumIdent.map(Integer) |
        VarIdent.map(Var)
    )

    // Definitions
    //===============
    def Stmt[$: P] = P(
        // ("var" ~ VarIdent ~ "=" ~ Expr).map{(v, e) =>Assign(Var(v), e)} | 
        ("write" ~/ Expr).map(Write) |
        ("def" ~/ VarIdent ~ "(" ~ VarIdent.rep(0, ",") ~ ")" ~ "=" ~/ Expr).map((v, a, e) => Assign(Var(v), Func(a, e))) |
        Expr
    )
    def Stmts[$: P] = Stmt.rep(1, ";")

    def parser[$: P] = Stmts ~ End
}
