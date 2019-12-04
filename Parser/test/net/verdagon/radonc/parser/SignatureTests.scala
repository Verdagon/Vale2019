package net.verdagon.radonc.parser

import org.scalatest.{FunSuite, Matchers}

class SignatureTests extends FunSuite with Matchers with Collector {
  private def compile[T](parser: VParser.Parser[T], code: String): T = {
    // The strip is in here because things inside the parser don't expect whitespace before and after
    VParser.parse(parser, code.strip().toCharArray()) match {
      case VParser.NoSuccess(msg, input) => {
        fail("Couldn't parse!\n" + input.pos.longString);
      }
      case VParser.Success(expr, rest) => {
        expr
      }
    }
  }

  // fn maxHp(this: virtual IUnit): Int;

  test("Impl function") {
    compile(
      VParser.topLevelFunction,
      "fn maxHp(this: Marine for IUnit) { 5 }") shouldEqual
        FunctionP(
          Some("maxHp"),false,false,true,List(),List(),
          List(
            PatternPP(
              Some(CaptureP("this",FinalP)),
              Some(NamePPT("Marine")),
              None,
              Some(OverrideP(NamePPT("IUnit"))))),
          None,
          Some(BlockPE(List(IntLiteralPE(5)))))
  }

  test("Param") {
    val program = compile(VParser.program, "fn call(f:F){f()}")
    program shouldHave PatternPP(Some(CaptureP("f",FinalP)),Some(NamePPT("F")),None,None)
  }

  test("Templated function") {
    compile(VParser.topLevelFunction, "fn sum () rules() {3}") shouldEqual
        FunctionP(Some("sum"),false,false,true,List(),List(), List(),None,Some(BlockPE(List(IntLiteralPE(3)))))
  }

  test("Identifying runes") {
    compile(
      VParser.topLevelFunction,
      "fn wrap:#F(a: #A) { }") shouldEqual
        FunctionP(
          Some("wrap"),false,false,true,
          List("F"),
          List(),
          List(Patterns.capturedWithTypeRune("a", "A")),
          None,
          Some(BlockPE(List(VoidPE()))))
  }
}
