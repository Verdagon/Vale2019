package net.verdagon.vale.scout.rules

import net.verdagon.vale.parser._
import net.verdagon.vale.scout._
import net.verdagon.vale.vfail
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
        programS.implementedFunctions.find(_.name == "main") match {
          case None => fail()
          case Some(main) => main.templateRules
        }
      }
    }
  }

  test("A") {
    val expectedRulesS =
      List(
        EqualsSR(
          TypedSR(Some("B"),CoordTypeSR),
          TemplexSR(CallST(NameST("List"),List(RuneST("A"))))),
        EqualsSR(
          TypedSR(Some("C"),CoordTypeSR),
          OrSR(List(TemplexSR(RuneST("B")), TemplexSR(RuneST("A")), TemplexSR(NameST("Int"))))),
        TypedSR(Some("A"),CoordTypeSR))
    RuleSUtils.getDistinctOrderedRunesForRulexes(expectedRulesS) shouldEqual List("B", "A", "C")
    compile(
      """fn main(a: #A)
        |rules(
        |  #B: Ref = List<#A>,
        |  #C: Ref = #B | #A | Int)
        |{ }
        |""".stripMargin) shouldEqual
      expectedRulesS
  }

  test("B") {
    val rulesS = compile("fn main() rules(#B<Ref> = List<#A>, #C<Ref> = #B | #A | Int) {}")
    RuleSUtils.getDistinctOrderedRunesForRulexes(rulesS) shouldEqual List("B", "A", "C")
  }
}
