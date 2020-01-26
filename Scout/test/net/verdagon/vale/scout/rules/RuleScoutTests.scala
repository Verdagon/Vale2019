package net.verdagon.vale.scout.rules

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.{vassert, vfail}
import org.scalatest.{FunSuite, Matchers}

class RuleScoutTests extends FunSuite with Matchers {
  private def compile(code: String): List[IRulexSR] = {
    VParser.parse(VParser.program, code.toCharArray()) match {
      case VParser.NoSuccess(msg, input) => {
        fail();
      }
      case VParser.Success(program0, rest) => {
        if (!rest.atEnd) {
          vfail(rest.pos.longString)
        }
        val programS = Scout.scoutProgram(program0)
        programS.lookupFunction("main").templateRules
      }
    }
  }

  val mainName =
    AbsoluteNameS(
      "in.vale",
      List(),
      FunctionNameS("main", CodeLocationS(0, 0)))

  test("A") {
    val expectedRulesS =
      List(
        EqualsSR(
          TypedSR(mainName.addStep(CodeRuneS("B")),CoordTypeSR),
          TemplexSR(CallST(NameST(ImpreciseNameS(List(), CodeTypeNameS("List"))),List(RuneST(mainName.addStep(CodeRuneS("A"))))))),
        EqualsSR(
          TypedSR(mainName.addStep(CodeRuneS("C")),CoordTypeSR),
          OrSR(List(TemplexSR(RuneST(mainName.addStep(CodeRuneS("B")))), TemplexSR(RuneST(mainName.addStep(CodeRuneS("A")))), TemplexSR(NameST(ImpreciseNameS(List(), CodeTypeNameS("Int"))))))),
        TypedSR(mainName.addStep(CodeRuneS("A")),CoordTypeSR))
    RuleSUtils.getDistinctOrderedRunesForRulexes(mainName, expectedRulesS) shouldEqual
      List(mainName.addStep(CodeRuneS("B")), mainName.addStep(CodeRuneS("A")), mainName.addStep(CodeRuneS("C")))

    val results =
      compile(
        """fn main<A>(a A)
          |rules(
          |  B Ref = List<A>,
          |  C Ref = B | A | Int)
          |{ }
          |""".stripMargin)
    results match {
      case List(
        EqualsSR(
          TypedSR(br1 @ AbsoluteNameS(_, List(FunctionNameS("main",_)),CodeRuneS("B")),CoordTypeSR),
          TemplexSR(CallST(NameST(ImpreciseNameS(_, CodeTypeNameS("List"))),List(RuneST(ar1 @ AbsoluteNameS(_, List(FunctionNameS("main",_)),CodeRuneS("A"))))))),
        EqualsSR(
          TypedSR(AbsoluteNameS(_, List(FunctionNameS("main",_)),CodeRuneS("C")),CoordTypeSR),
          OrSR(List(TemplexSR(RuneST(br2)), TemplexSR(RuneST(ar3)), TemplexSR(NameST(ImpreciseNameS(List(), CodeTypeNameS("Int"))))))),
        TypedSR(ar2,CoordTypeSR)) => {
        vassert(br1 == br2)
        vassert(ar1 == ar2)
        vassert(ar1 == ar3)
      }
    }
  }

  test("B") {
    val rulesS = compile("fn main() rules(B Ref = List<A>, A Ref, C Ref = B | A | Int) {}")
    RuleSUtils.getDistinctOrderedRunesForRulexes(mainName, rulesS) match {
      case List(
        AbsoluteNameS(_, List(FunctionNameS("main",_)),CodeRuneS("B")),
        AbsoluteNameS(_, List(FunctionNameS("main",_)),CodeRuneS("A")),
        AbsoluteNameS(_, List(FunctionNameS("main",_)),CodeRuneS("C"))) =>
    }
  }
}
