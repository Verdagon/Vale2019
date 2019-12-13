package net.verdagon.vale.parser.rules

import net.verdagon.vale.parser.VParser._
import net.verdagon.vale.parser._
import net.verdagon.vale.vfail
import org.scalatest.{FunSuite, Matchers}

class RuleTests extends FunSuite with Matchers {
  private def compile[T](parser: VParser.Parser[T], code: String): T = {
    VParser.parse(parser, code.toCharArray()) match {
      case VParser.NoSuccess(msg, input) => {
        fail();
      }
      case VParser.Success(expr, rest) => {
        if (!rest.atEnd) {
          vfail(rest.pos.longString)
        }
        expr
      }
    }
  }
  private def compile[T](code: String): PatternPP = {
    compile(atomPattern, code)
  }

  private def checkFail[T](parser: VParser.Parser[T], code: String) = {
    VParser.parse(parser, "") match {
      case VParser.NoSuccess(_, _) =>
      case VParser.Success(_, rest) => {
        if (!rest.atEnd) {
          fail(rest.pos.longString)
        }
        fail()
      }
    }
  }

  test("Nothing matches empty string") {
    checkFail(prototypeRulePR, "")
    checkFail(callableRulePR, "")
    checkFail(existsPR, "")
    checkFail(implementsPR, "")
    checkFail(keywordOrIdentifierOrRuneRuleTemplexPR, "")
    checkFail(keywordOrIdentifierRuleTemplexPR, "")
    checkFail(level0PR, "")
    checkFail(level1PR, "")
    checkFail(level2PR, "")
    checkFail(level3PR, "")
    checkFail(level4PR, "")
    checkFail(level5PR, "")
    checkFail(manualSeqRulePR, "")
    checkFail(packRulePR, "")
    checkFail(repeaterSeqRulePR, "")
    checkFail(rulePR, "")
    checkFail(ruleTemplexPR, "")
    checkFail(ruleTemplexPR, "")
    checkFail(ruleTemplexSetPR, "")
    checkFail(runeRuleTemplexPR, "")
    checkFail(templateRulesPR, "")
  }

  test("Relations") {
    compile(rulePR, "implements(MyObject, IObject)") shouldEqual
        CallPR("implements",List(TemplexPR(NamePRT("MyObject")), TemplexPR(NamePRT("IObject"))))
    compile(rulePR, "implements(#R, IObject)") shouldEqual
        CallPR("implements",List(TemplexPR(RunePRT("R")), TemplexPR(NamePRT("IObject"))))
    compile(rulePR, "implements(MyObject, #T)") shouldEqual
        CallPR("implements",List(TemplexPR(NamePRT("MyObject")), TemplexPR(RunePRT("T"))))
    compile(rulePR, "exists(fn +(#T)Int)") shouldEqual
        CallPR("exists", List(TemplexPR(PrototypePRT("+", List(RunePRT("T")), NamePRT("Int")))))
  }

  test("Super complicated") {
    compile(rulePR, "#C = [#I * #X] | [#N * #T]") // succeeds
  }
}
