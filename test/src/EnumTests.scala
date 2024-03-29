package compiler
  
import org.scalatest.flatspec.AnyFlatSpec
import ValParser._
import PreProcess._

class EnumSpec extends AnyFlatSpec {

    behavior of "Pre-process step"

    it should "correctly extract an enum definition" in {
        val en = EnumDef("A", List("B", "C"))
        val input = Sequence(en, Num(5))
        val (enums, exp) = extract_enums(input)
        assert(enums == Map("A" -> EnumType(List("B", "C"))))
        assert(exp == Num(5))
    }

    it should "correctly extract multiple enum definitions across multiple sequences" in {
        val en1 = EnumDef("A", List("B", "C"))
        val en2 = EnumDef("D", List("E", "F"))
        val input = Sequence(en1, Sequence(Num(5), en2))
        val (enums, exp) = extract_enums(input)
        assert(enums == Map("A" -> EnumType(List("B", "C")), "D" -> EnumType(List("E", "F"))))
        assert(exp == Num(5))
    }

    behavior of "Match to If transformation"

    it should "correctly transform a match statement with a default case" in {
        val m = Match("a", List(MCase("A", "B", Num(6)), MCase("", "_", Num(5))))
        val expected = If(Op(Var("a"), "==", EnumRef("A", "B")), Num(6), Some(Num(5)))
        assert(transform_match_to_if(m) == expected)
    }

    it should "correctly transform a match statement with multiple cases" in {
        val m = Match("a", List(MCase("A", "B", Num(6)), MCase("C", "D", Num(5))))
        val expected = If(Op(Var("a"), "==", EnumRef("A", "B")), Num(6), Some(If(Op(Var("a"), "==", EnumRef("C", "D")), Num(5), Some(Op(Num(0), "+", Num(0))))))
        assert(transform_match_to_if(m) == expected)
    }

    it should "correctly transform a match statement with multiple cases and a default case" in {
        val m = Match("a", List(MCase("A", "B", Num(6)), MCase("C", "D", Num(5)), MCase("", "_", Num(4))))
        val expected = If(Op(Var("a"), "==", EnumRef("A", "B")), Num(6), Some(If(Op(Var("a"), "==", EnumRef("C", "D")), Num(5), Some(Num(4)))))
        assert(transform_match_to_if(m) == expected)
    }

    it should "correctly skip other cases after a default case" in {
        val m = Match("a", List(MCase("A", "B", Num(6)), MCase("", "_", Num(5)), MCase("C", "D", Num(4))))
        val expected = If(Op(Var("a"), "==", EnumRef("A", "B")), Num(6), Some(Num(5)))
        assert(transform_match_to_if(m) == expected)
    }

}
