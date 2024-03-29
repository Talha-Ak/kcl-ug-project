package compiler
  
import org.scalatest.flatspec.AnyFlatSpec
import ValParser._
import fastparse.parse

class ParserSpec extends AnyFlatSpec {

    val parseExp = (name: String) => fastparse.parse(name, Exp(_))
    val parseType = (name: String) => fastparse.parse(name, TypeParser(_))
    val parseProg = (name: String) => fastparse.parse(name, Prog(_))

    behavior of "Values"

    it should "corretly recognise integers" in {
        assert(parseExp("5").get.value == Num(5))
    }

    it should "correctly recognise floats" in {
        assert(parseExp("5.0").get.value == Flt(5.0))
    }

    it should "correctly recognise booleans" in {
        assert(parseExp("true").get.value == Bool(true))
    }

    it should "correctly recognise variables" in {
        assert(parseExp("x").get.value == Var("x"))
    }

    behavior of "Arithmatic expressions"

    it should "have left to right precedence" in {
        assert(parseExp("2 - 3 + 1").get.value == Op(Op(Num(2), "-", Num(3)), "+", Num(1)))
    }

    it should "respect order of operations" in {
        assert(parseExp("2 + (3 - 2) * 4").get.value == Op(Num(2), "+", Op(Op(Num(3), "-", Num(2)), "*", Num(4))))
    }

    it should "work with both numbers and variables" in {
        assert(parseExp("2 % (x + y)").get.value == Op(Num(2), "%", Op(Var("x"), "+", Var("y"))))
    }

    it should "work with function calls" in {
        assert(parseExp("5 + square(2) / i").get.value == Op(Num(5), "+", Op(Call("square", List(Num(2))), "/", Var("i"))))
    }

    behavior of "Boolean expressions"

    it should "respect precedence" in {
        assert(parseExp("5 <= 2 == 2 * 3 > 4").get.value == Op(Op(Num(5), "<=", Num(2)), "==", Op(Op(Num(2), "*", Num(3)), ">", Num(4))))
    }

    it should "work with arithmatic expressions" in {
        assert(parseExp("2 + 2 == 3 - 4 * fact(n)").get.value == Op(Op(Num(2), "+", Num(2)), "==", Op(Num(3), "-", Op(Num(4), "*", Call("fact", List(Var("n")))))))
    }

    behavior of "If expressions"

    it should "work with boolean expressions" in {
        assert(parseExp("if 2 + 2 == 4 then 1 else 0").get.value == If(Op(Op(Num(2), "+", Num(2)), "==", Num(4)), Num(1), Some(Num(0))))
    }

    it should "work with boolean variables" in {
        assert(parseExp("if x then 1 else 0").get.value == If(Var("x"), Num(1), Some(Num(0))))
    }

    it should "work with function calls" in {
        assert(parseExp("if fact(n) then 1 else 0").get.value == If(Call("fact", List(Var("n"))), Num(1), Some(Num(0))))
    }

    it should "be able to chain multiple if statements" in {
        assert(parseExp("if x then 1 else if y then 2 else 3").get.value == If(Var("x"), Num(1), Some(If(Var("y"), Num(2), Some(Num(3))))))
    }

    behavior of "Functions"

    it should "work with multiple arguments in calls" in {
        assert(parseExp("f(1, 2, 3)").get.value == Call("f", List(Num(1), Num(2), Num(3))))
    }

    it should "work with variables" in {
        assert(parseExp("f(x, y, z)").get.value == Call("f", List(Var("x"), Var("y"), Var("z"))))
    }

    it should "work with function calls" in {
        assert(parseExp("f(g(x), h(y))").get.value == Call("f", List(Call("g", List(Var("x"))), Call("h", List(Var("y"))))))
    }

    it should "work with multiple arguments in definition" in {
        assert(parse("def f(x: Int, y: Int, z: Int): Int = x + y + z;", DefFn).get.value == Func("f", List(("x", IntType), ("y", IntType), ("z", IntType)), IntType, Op(Op(Var("x"), "+", Var("y")), "+", Var("z"))))
    }

    behavior of "Match expressions"

    it should "work with enum references" in {
        assert(parseExp("x match {\ncase T::A => 1\ncase T::B => 2\n}").get.value == Match("x", List(MCase("T", "A", Num(1)), MCase("T", "B", Num(2)))))
    }

    it should "work with default values" in {
        assert(parseExp("x match {\ncase T::A => 1\ncase _ => 2\n}").get.value == Match("x", List(MCase("T", "A", Num(1)), MCase("", "_", Num(2)))))
    }

    behavior of "Block expressions"

    it should "work with multiple expressions" in {
        assert(parseExp("{\nval x: Int = 5;\nx + 1\n}").get.value == Sequence(Const("x", IntType, Num(5)), Op(Var("x"), "+", Num(1))))
    }

    behavior of "Type hints"

    it should "work with function types" in {
        assert(parseType("(Int, Int) => Int").get.value == FnType(List(IntType, IntType), IntType))
    }

    it should "work with user defined types" in {
        assert(parseType("MyType").get.value == UserType("MyType"))
    }

    it should "work with function types with user defined types" in {
        assert(parseType("(MyType, Int) => MyType").get.value == FnType(List(UserType("MyType"), IntType), UserType("MyType")))
    }

    behavior of "Structs"

    it should "work with struct references" in {
        assert(parseExp("x.y").get.value == StructRef("x", "y"))
    }

    it should "work with struct defs" in {
        assert(parse("struct MyStruct = { x: Int, y: Int };", DefStruct).get.value == StructDef("MyStruct", List(("x", IntType),("y", IntType))))
    }

    behavior of "Enums"

    it should "work with enum references" in {
        assert(parseExp("T::A").get.value == EnumRef("T", "A"))
    }

    it should "work with enum defs" in {
        assert(parse("enum MyEnum = A | B | C;", DefEnum).get.value == EnumDef("MyEnum", List("A", "B", "C")))
    }

}
