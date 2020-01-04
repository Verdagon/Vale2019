package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.patterns.AtomSP
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.vfail
import org.scalatest.{FunSuite, Matchers}

class ScoutParametersTests extends FunSuite with Matchers {
  private def compile(code: String): ProgramS = {
    VParser.parse(VParser.program, code.toCharArray()) match {
      case VParser.NoSuccess(msg, input) => {
        fail();
      }
      case VParser.Success(program0, rest) => {
        if (!rest.atEnd) {
          vfail(rest.pos.longString)
        }
        Scout.scoutProgram(program0)
      }
    }
  }

  test("Simple rune rule") {
    val program1 = compile("""fn main<T>(moo T) { }""")
    val main = program1.lookupFunction("main")
    val expectedRulesS = List(TypedSR(Some("T"),CoordTypeSR))
    RuleSUtils.getDistinctOrderedRunesForRulexes(expectedRulesS) shouldEqual List("T")
    main.templateRules shouldEqual expectedRulesS
  }

  test("Borrowed rune") {
    val program1 = compile("""fn main<T>(moo &T) { }""")
    val main = program1.lookupFunction("main")
    val List(param) = main.params
    param shouldEqual
        ParameterS(AtomSP(Some(CaptureP("moo",FinalP)),None,"__ParamRune_0",None))

    val expectedRulesS =
      List(
        TypedSR(Some("T"),CoordTypeSR),
        TypedSR(Some("__ParamRune_0K"),KindTypeSR),
        ComponentsSR(
          TypedSR(Some("T"),CoordTypeSR),
          List(
            TemplexSR(OwnershipST(OwnP)),
            TemplexSR(RuneST("__ParamRune_0K")))),
        TypedSR(
          Some("__ParamRune_0"),CoordTypeSR),
        ComponentsSR(
          TypedSR(Some("__ParamRune_0"),CoordTypeSR),
          List(
            TemplexSR(OwnershipST(BorrowP)),
            TemplexSR(RuneST("__ParamRune_0K")))))

    RuleSUtils.getDistinctOrderedRunesForRulexes(expectedRulesS) shouldEqual
      List("T", "__ParamRune_0K", "__ParamRune_0")

    main.templateRules shouldEqual expectedRulesS
  }

  test("Anonymous typed param") {
    val program1 = compile("""fn main(_ Int) { }""")
    val main = program1.lookupFunction("main")
    val List(param) = main.params
    param shouldEqual
        ParameterS(AtomSP(Some(CaptureP("__param_0",FinalP)),None,"__ParamRune_0",None))

    val expectedRulesS =
      List(
        TypedSR(Some("__ParamRune_0"),CoordTypeSR),
        EqualsSR(
          TemplexSR(RuneST("__ParamRune_0")),
          TemplexSR(NameST("Int"))))

    RuleSUtils.getDistinctOrderedRunesForRulexes(expectedRulesS) shouldEqual
      List("__ParamRune_0")

    main.templateRules shouldEqual expectedRulesS
  }

  test("Anonymous untyped param") {
    val program1 = compile("""fn main(_) { }""")
    val main = program1.lookupFunction("main")
    val List(param) = main.params
    param shouldEqual
        ParameterS(AtomSP(Some(CaptureP("__param_0",FinalP)),None,"__ParamRune_0_0",None))

    val expectedRulesS =
      List(TypedSR(Some("__ParamRune_0_0"),CoordTypeSR))

    RuleSUtils.getDistinctOrderedRunesForRulexes(expectedRulesS) shouldEqual
      List("__ParamRune_0_0")

    main.templateRules shouldEqual expectedRulesS

    main.identifyingRunes shouldEqual List("__ParamRune_0_0")
  }

  test("Rune destructure") {
    // This is an ambiguous case but we decided it should destructure a struct or sequence, see CSTODTS in docs.
    val program1 = compile("""fn main<T>(moo T(a Int)) { }""")
    val main = program1.lookupFunction("main")

    val aRune = Scout.unrunedParamRunePrefix + 0 + Scout.memberRuneSeparator + "0"

    val List(param) = main.params
    param shouldEqual
        ParameterS(
          AtomSP(
            Some(CaptureP("moo",FinalP)),
            None,
            "T",
            Some(List(AtomSP(Some(CaptureP("a",FinalP)),None,aRune,None)))))

    val expectedRulesS =
      List(
        TypedSR(Some("T"),CoordTypeSR),
        TypedSR(Some(aRune),CoordTypeSR),
        EqualsSR(TemplexSR(RuneST(aRune)),TemplexSR(NameST("Int"))))

    RuleSUtils.getDistinctOrderedRunesForRulexes(expectedRulesS) shouldEqual
      List("T", aRune)

    main.templateRules shouldEqual expectedRulesS

    // Yes, even though the user didnt specify any. See CCAUIR.
    main.identifyingRunes shouldEqual List("T")
  }
}
