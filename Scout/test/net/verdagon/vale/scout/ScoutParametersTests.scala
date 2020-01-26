package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.patterns.{AtomSP, CaptureS}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.{vassert, vfail}
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

  val mainName =
    AbsoluteNameS(
      "in.vale",
      List(),
      FunctionNameS("main", CodeLocationS(0, 0)))

  test("Simple rune rule") {
    val program1 = compile("""fn main<T>(moo T) { }""")
    val main = program1.lookupFunction("main")

    val runeInRules =
      main.templateRules match {
        case List(TypedSR(rune @ AbsoluteNameS(_, List(FunctionNameS("main", _)), CodeRuneS("T")),CoordTypeSR)) => rune
      }
    RuleSUtils.getDistinctOrderedRunesForRulexes(mainName, main.templateRules) match {
      case List(runeFromFunc) => vassert(runeInRules == runeFromFunc)
    }
  }

  test("Borrowed rune") {
    val program1 = compile("""fn main<T>(moo &T) { }""")
    val main = program1.lookupFunction("main")
    val List(param) = main.params

    val tCoordRune =
      param match {
        case ParameterS(
          AtomSP(
            CaptureS(AbsoluteNameS(_, _, CodeVarNameS("moo")),FinalP),
            None,
            tcr @ AbsoluteNameS(_, _, ImplicitRuneS(1)),
            None)) => tcr
      }

    main.templateRules match {
      case List(
        TypedSR(AbsoluteNameS(_,_,ImplicitRuneS(0)),KindTypeSR),
        TypedSR(AbsoluteNameS(_,_,CodeRuneS("T")),CoordTypeSR),
        TypedSR(AbsoluteNameS(_,_,ImplicitRuneS(1)),CoordTypeSR),
        ComponentsSR(
          TypedSR(AbsoluteNameS(_,_,CodeRuneS("T")),CoordTypeSR),
          List(
            TemplexSR(OwnershipST(OwnP)),
            TemplexSR(RuneST(AbsoluteNameS(_,_,ImplicitRuneS(0)))))),
        ComponentsSR(
          TypedSR(AbsoluteNameS(_,_,ImplicitRuneS(1)),CoordTypeSR),
          List(
            TemplexSR(OwnershipST(BorrowP)),
            TemplexSR(RuneST(AbsoluteNameS(_,_,ImplicitRuneS(0))))))) =>
    }

    RuleSUtils.getDistinctOrderedRunesForRulexes(mainName, main.templateRules) match {
      case List(
        AbsoluteNameS(_,_,ImplicitRuneS(0)),
          AbsoluteNameS(_,_,CodeRuneS("T")),
          AbsoluteNameS(_,_,ImplicitRuneS(1))) =>
    }
  }

  test("Anonymous typed param") {
    val program1 = compile("""fn main(_ Int) { }""")
    val main = program1.lookupFunction("main")
    val List(param) = main.params
    val paramRune =
      param match {
        case ParameterS(
          AtomSP(
            CaptureS(AbsoluteNameS(_, List(FunctionNameS("main",_)), UnnamedLocalNameS(_)),FinalP),
            None,
            pr @ AbsoluteNameS(_, List(FunctionNameS("main",_)),ImplicitRuneS(0)),
            None)) => pr
      }

    main.templateRules match {
      case List(
        EqualsSR(
          TypedSR(pr,CoordTypeSR),
          TemplexSR(NameST(ImpreciseNameS(List(), CodeTypeNameS("Int")))))) => {
        vassert(pr == paramRune)
      }
    }

    RuleSUtils.getDistinctOrderedRunesForRulexes(mainName, main.templateRules) shouldEqual
      List(paramRune)
  }

  test("Anonymous untyped param") {
    val program1 = compile("""fn main(_) { }""")
    val main = program1.lookupFunction("main")
    val List(param) = main.params
    val paramRune =
      param match {
        case ParameterS(
         AtomSP(
          CaptureS(AbsoluteNameS(_, List(FunctionNameS("main",_)), UnnamedLocalNameS(_)),FinalP),
          None,
          pr @ AbsoluteNameS(_, List(FunctionNameS("main",_)),ImplicitRuneS(0)),
          None)) => pr
      }

    main.templateRules match {
      case List(TypedSR(pr,CoordTypeSR)) => {
        vassert(pr == paramRune)
      }
    }

    RuleSUtils.getDistinctOrderedRunesForRulexes(mainName, main.templateRules) shouldEqual
      List(paramRune)
  }

  test("Rune destructure") {
    // This is an ambiguous case but we decided it should destructure a struct or sequence, see CSTODTS in docs.
    val program1 = compile("""fn main<T>(moo T(a Int)) { }""")
    val main = program1.lookupFunction("main")

    val List(param) = main.params

    val (aRune, tRune) =
      param match {
        case ParameterS(
            AtomSP(
              CaptureS(AbsoluteNameS(_, List(FunctionNameS("main",_)), CodeVarNameS("moo")),FinalP),
              None,
              tr @ AbsoluteNameS(_, List(FunctionNameS("main",_)), CodeRuneS("T")),
              Some(
                List(
                  AtomSP(
                    CaptureS(AbsoluteNameS(_, List(FunctionNameS("main",_)), CodeVarNameS("a")),FinalP),
                    None,
                    ar @ AbsoluteNameS(_, List(FunctionNameS("main",_)), ImplicitRuneS(0)),
                    None))))) => (ar, tr)
      }

    main.templateRules match {
      case List(
        TypedSR(tr,CoordTypeSR),
        EqualsSR(
          TypedSR(ar,CoordTypeSR),
          TemplexSR(NameST(ImpreciseNameS(List(), CodeTypeNameS("Int")))))) => {
        vassert(tr == tRune)
        vassert(ar == aRune)
      }
    }

    RuleSUtils.getDistinctOrderedRunesForRulexes(mainName, main.templateRules) shouldEqual
      List(tRune, aRune)

    // See CCAUIR.
    main.identifyingRunes shouldEqual List(tRune)
  }
}
