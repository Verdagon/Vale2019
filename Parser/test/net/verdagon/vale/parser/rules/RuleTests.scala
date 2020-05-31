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
    checkFail(templateRulesPR, "")
    checkFail(packPR, "")
  }

  test("Relations") {
    compile(rulePR, "implements(MyObject, IObject)") shouldEqual
        CallPR("implements",List(TemplexPR(NameOrRunePRT("MyObject")), TemplexPR(NameOrRunePRT("IObject"))))
    compile(rulePR, "implements(R, IObject)") shouldEqual
        CallPR("implements",List(TemplexPR(NameOrRunePRT("R")), TemplexPR(NameOrRunePRT("IObject"))))
    compile(rulePR, "implements(MyObject, T)") shouldEqual
        CallPR("implements",List(TemplexPR(NameOrRunePRT("MyObject")), TemplexPR(NameOrRunePRT("T"))))
    compile(rulePR, "exists(fn +(T)Int)") shouldEqual
        CallPR("exists", List(TemplexPR(PrototypePRT("+", List(NameOrRunePRT("T")), NameOrRunePRT("Int")))))
  }

  test("Super complicated") {
    compile(rulePR, "C = [I * X] | [N * T]") // succeeds
  }
//
//  test("resolveExactSignature") {
//    compile(rulePR, "C = resolveExactSignature(\"__call\", (&F, Int))") shouldEqual
//      EqualsPR(
//        TemplexPR(NameOrRunePRT("C")),
//        CallPR(
//          "resolveExactSignature",
//          List(
//            TemplexPR(StringPRT("__call")),
//            PackPR(List(TemplexPR(BorrowPRT(NameOrRunePRT("F"))), TemplexPR(NameOrRunePRT("Int")))))))
//  }

  test("destructure prototype") {
    compile(rulePR, "Prot(_, _, T) = moo") shouldEqual
      EqualsPR(
        ComponentsPR(
          TypedPR(None,PrototypeTypePR),
          List(TemplexPR(AnonymousRunePRT()), TemplexPR(AnonymousRunePRT()), TemplexPR(NameOrRunePRT("T")))),
        TemplexPR(NameOrRunePRT("moo")))
  }

  test("prototype with coords") {
    compile(rulePR, "Prot(_, (Int, Bool), _)") shouldEqual
      ComponentsPR(
        TypedPR(None,PrototypeTypePR),
        List(
          TemplexPR(AnonymousRunePRT()),
          TemplexPR(PackPRT(List(NameOrRunePRT("Int"), NameOrRunePRT("Bool")))),
          TemplexPR(AnonymousRunePRT())))
  }
}
