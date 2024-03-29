package compiler
  
import org.scalatest.flatspec.AnyFlatSpec
import ValParser._

class CPSSpec extends AnyFlatSpec {

    val MockCounter = new Counter {
        def Fresh(x: String) = x
    }
    val cps = (exp: Exp) => NewCPS(MockCounter).CPSi(exp)

    behavior of "Exp to KAnf conversion"

    it should "correctly convert a simple expression" in {
        assert(cps(Num(5)) == KReturn(KNum(5)))
    }

    it should "correctly convert a simple boolean" in {
        assert(cps(Bool(true)) == KReturn(KBool(true)))
    }

    it should "correctly convert a simple float" in {
        assert(cps(Flt(5.0)) == KReturn(KFloat(5.0)))
    }

    it should "correctly convert an if expression" in {
        val if_exp = If(Op(Num(2), "<", Num(3)), Num(5), Some(Num(6)))
        val expected = KLet("tmp", Kop("<", KNum(2), KNum(3)), KIf("tmp", KReturn(KNum(5)), KReturn(KNum(6))))
        assert(cps(if_exp) == expected)
    }

    it should "correctly convert a sequence" in {
        assert(cps(Sequence(Num(5), Num(6))) == KReturn(KNum(6)))
    }

    it should "correctly convert a typed const expression" in {
        assert(cps(Const("x", IntType, Num(5))) == KConst("x", KNum(5), KReturn(KVar("x", IntType))))
    }

    it should "correctly convert a function" in {
        val func = Func("f", List(("x", IntType)), IntType, Num(5))
        val expected = KFun("f", List(("x", IntType)), IntType, KReturn(KNum(5)), KReturn(KVar("f", FnType(List(IntType), IntType))))
        assert(cps(func) == expected)
    }

    it should "correctly convert a function call after definition" in {
        val func = Func("f", List(("x", IntType)), IntType, Num(5))
        val call = Call("f", List(Num(5)))
        val expected = KFun("f", List(("x", IntType)), IntType, KReturn(KNum(5)), KLet("tmp", KCall(KVar("f", FnType(List(IntType), IntType)), List(KNum(5))), KReturn(KVar("tmp", IntType))))
        assert(cps(Sequence(func, call)) == expected)
    }

    it should "correctly convert an enum reference" in {
        assert(cps(EnumRef("A", "B")) == KReturn(KEnum("A", "B")))
    }

    it should "correctly convert a struct definition" in {
        assert(cps(StructDef("A", List(("x", IntType), ("y", BoolType)))) == KStructDef(Struct("A", List(("x", IntType), ("y", BoolType))), KReturn(KVar("A", UserType("A")))))
    }

    it should "correctly convert a struct reference after a struct definition" in {
        val struct = StructDef("A", List(("x", IntType), ("y", BoolType)))
        val ref = StructRef("A", "x")
        val expected = KStructDef(Struct("A", List(("x", IntType), ("y", BoolType))), KStructRef("tmp", "A", "x", UserType("A"), KReturn(KVar("tmp", IntType))))
        assert(cps(Sequence(struct, ref)) == expected)
    }
}
