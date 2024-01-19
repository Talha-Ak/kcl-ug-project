package compiler
  
import org.scalatest.flatspec.AnyFlatSpec
import ValParser._

class ParserSpec extends AnyFlatSpec {

    val parse = (name: String) => fastparse.parse(name, Exp(_))

    behavior of "Arithmatic expressions"

    it should "have left to right precedence" in {
        assert(parse("2 - 3 + 1").get.value == Op(Op(Num(2), "-", Num(3)), "+", Num(1)))
    }

    it should "respect order of operations" in {
        assert(parse("2 + (3 - 2) * 4").get.value == Op(Num(2), "+", Op(Op(Num(3), "-", Num(2)), "*", Num(4))))
    }

    it should "work with both numbers and variables" in {
        assert(parse("2 % (x + y)").get.value == Op(Num(2), "%", Op(Var("x"), "+", Var("y"))))
    }

    it should "work with function calls" in {
        assert(parse("5 + square(2) / i").get.value == Op(Num(5), "+", Op(Call("square", List(Num(2))), "/", Var("i"))))
    }

    behavior of "Boolean expressions"

    it should "respect precedence" in {
        assert(parse("5 <= 2 == 2 * 3 > 4").get.value == Op(Op(Num(5), "<=", Num(2)), "==", Op(Op(Num(2), "*", Num(3)), ">", Num(4))))
    }

    it should "work with arithmatic expressions" in {
        assert(parse("2 + 2 == 3 - 4 * fact(n)").get.value == Op(Op(Num(2), "+", Num(2)), "==", Op(Num(3), "-", Op(Num(4), "*", Call("fact", List(Var("n")))))))
    }

}
