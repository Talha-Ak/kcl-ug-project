package compiler
  
import org.scalatest.flatspec.AnyFlatSpec
import parser._

class ParserSpec extends AnyFlatSpec {

    val parse = (name: String) => fastparse.parse(name, Expr(_))

    behavior of "Arithmatic expressions"

    it should "have left to right precedence" in {
        assert(parse("2 - 3 + 1").get.value == Op(Op(Integer(2), "-", Integer(3)), "+", Integer(1)))
    }

    it should "respect order of operations" in {
        assert(parse("2 + (3 - 2) * 4").get.value == Op(Integer(2), "+", Op(Op(Integer(3), "-", Integer(2)), "*", Integer(4))))
    }

    it should "work with both numbers and variables" in {
        assert(parse("2 % (x + y)").get.value == Op(Integer(2), "%", Op(Var("x"), "+", Var("y"))))
    }

    it should "work with function calls" in {
        assert(parse("5 + square(2) / i").get.value == Op(Integer(5), "+", Op(Call("square", List(Integer(2))), "/", Var("i"))))
    }

    behavior of "Boolean expressions"

    it should "respect precedence" in {
        assert(parse("5 <= 2 == 2 * 3 > 4").get.value == Op(Op(Integer(5), "<=", Integer(2)), "==", Op(Op(Integer(2), "*", Integer(3)), ">", Integer(4))))
    }

    it should "work with arithmatic expressions" in {
        assert(parse("2 + 2 == 3 - 4 * fact(n)").get.value == Op(Op(Integer(2), "+", Integer(2)), "==", Op(Integer(3), "-", Op(Integer(4), "*", Call("fact", List(Var("n")))))))
    }

    behavior of "Unary operators"

    it should "work with any expression type" in {
        assert(parse("!(5 >= 2)").get.value == UnaryOp("!", Op(Integer(5), ">=", Integer(2))))
        assert(parse("-6 * 2 + 4").get.value == Op(Op(UnaryOp("-", Integer(6)), "*", Integer(2)), "+", Integer(4)))

    }

}
