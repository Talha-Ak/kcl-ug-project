package compiler
  
import org.scalatest.flatspec.AnyFlatSpec
import ValParser._
import PreProcess._

class ClosureSpec extends AnyFlatSpec {

    val MockCounter = new Counter {
        def Fresh(x: String) = x
    }
    val closure = new ClosureConv(MockCounter)
    import closure._

    behavior of "Detecting free variables"

    it should "correctly extract free variables from a KVal" in {
        val v = KVar("x")
        assert(free_val(v) == Set(KVar("x")))
    }

    it should "correctly extract free variables from a KExp" in {
        val v1 = KVar("x")
        val v2 = KVar("y")
        val e = Kop("+", v1, v2)
        assert(free_exp(e) == Set(KVar("x"), KVar("y")))
    }

    it should "correctly extract free variables from a KAnf" in {
        val v1 = KVar("x")
        val v2 = KVar("y")
        val e1 = Kop("+", v1, v2)
        val e = KLet("a", e1, KConst("b", v2, KReturn(v1)))
        assert(free_anf(e) == Set(KVar("x"), KVar("y")))
    }

    behavior of "Converting to closure form"

    it should "correctly convert a function with no free variables" in {
        val v1 = KVar("x")
        val v2 = KVar("y")
        val e1 = KLet("tmp", Kop("+", v1, v2), KReturn(KVar("tmp")))
        val e = KFun("f", List(("x", IntType), ("y", IntType)), IntType, e1, KReturn(v1))
        val (converted, _) = convert(e)
        assert(converted == KFun("f", List(("x", IntType), ("y", IntType)), IntType, e1, KReturn(v1)))
    }

    it should "correctly convert a function with free variables" in {
        val v1 = KVar("x")
        val v2 = KVar("y")
        val e1 = KLet("tmp", Kop("+", v1, v2), KReturn(KVar("tmp")))
        val e = KFun("f", List(("x", IntType)), IntType, e1, KReturn(KVar("x")))
        val (converted, _) = convert(e)
        val expected =
            KFun("f", List(("env", EnvType("env")), ("x", IntType)), IntType,
                KLetEnvRef("y", Ref(KVar("env", EnvType("env")), 1),
                    KLet("tmp", Kop("+", v1, v2),
                        KReturn(KVar("tmp"))
                    )
                ),
                KLetEnv("f", Env("env", List(KVar("f", FnType(List(EnvType("env"), IntType), IntType)), KVar("y"))),
                    KReturn(KVar("x"))
                )
            )
        assert(converted == expected)
    }

}
