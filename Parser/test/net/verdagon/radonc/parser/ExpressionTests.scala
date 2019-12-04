package net.verdagon.radonc.parser

import net.verdagon.radonc.vassert
import org.scalatest.{FunSuite, Matchers}

class ExpressionTests extends FunSuite with Matchers {
  private def compile(code: String): IExpressionPE = {
    VParser.parse(VParser.expression, code.toCharArray()) match {
      case VParser.NoSuccess(msg, input) => {
        fail();
      }
      case VParser.Success(expr, rest) => {
        vassert(rest.atEnd)
        expr
      }
    }
  }

  test("PE") {
    compile("4") shouldEqual IntLiteralPE(4)
  }

  test("2") {
    compile("4 + 5") shouldEqual FunctionCallPE(LookupPE("+", List()), PackPE(List(IntLiteralPE(4), IntLiteralPE(5))),true);
  }

  test("Floats") {
    compile("4.2") shouldEqual FloatLiteralPE(4.2f)
  }

  test("3") {
    compile("()") shouldEqual PackPE(List());
  }

  test("4") {
    compile("+(4, 5)") shouldEqual FunctionCallPE(LookupPE("+", List()), PackPE(List(IntLiteralPE(4), IntLiteralPE(5))),true);
  }

  test("5") {
    compile("x(y)") shouldEqual FunctionCallPE(LookupPE("x", List()), PackPE(List(LookupPE("y", List()))),true);
  }

  test("6") {
    compile("not y") shouldEqual FunctionCallPE(LookupPE("not", List()), PackPE(List(LookupPE("y", List()))),true);
  }

  test("Lending result of function call") {
    compile("&Muta()") shouldEqual LendPE(FunctionCallPE(LookupPE("Muta", List()), PackPE(List()),true));
  }

  test("Method call") {
    compile("x.shout()") shouldEqual
      FunctionCallPE(DotPE(LookupPE("x",List()),LookupPE("shout",List()),true),PackPE(List()),true)
  }

  test("Moving method call") {
    compile("x^.shout()") shouldEqual
      FunctionCallPE(DotPE(LookupPE("x",List()),LookupPE("shout",List()),false),PackPE(List()),true)
  }

  test("Templated function call") {
    compile("toArray:imm(&result)") shouldEqual
      FunctionCallPE(
        LookupPE("toArray",List(MutabilityPT(ImmutableP))),
        PackPE(List(LendPE(LookupPE("result",List())))),
        true)
  }

  test("Templated method call") {
    compile("result.toArray:imm()") shouldEqual
      FunctionCallPE(
        DotPE(
          LookupPE("result",List()),
          LookupPE("toArray",List(MutabilityPT(ImmutableP))),true),
        PackPE(List()),true)
  }

  test("Custom binaries") {
    compile("not y florgle not x") shouldEqual
        FunctionCallPE(
          LookupPE("florgle", List()),
          PackPE(List(
            FunctionCallPE(
              LookupPE("not", List()),
              PackPE(List(LookupPE("y", List()))),
              true),
            FunctionCallPE(
              LookupPE("not", List()),
              PackPE(List(LookupPE("x", List()))),
              true))),
          true)
  }

  test("Custom with noncustom binaries") {
    compile("a + b florgle x * y") shouldEqual
        FunctionCallPE(
          LookupPE("florgle", List()),
          PackPE(List(
            FunctionCallPE(
              LookupPE("+", List()),
              PackPE(List(LookupPE("a", List()), LookupPE("b", List()))),
              true),
            FunctionCallPE(
              LookupPE("*", List()),
              PackPE(List(LookupPE("x", List()), LookupPE("y", List()))),
              true))),
          true)
  }

  test("Template calling") {
    compile("MyNone:Int()") shouldEqual
      FunctionCallPE(LookupPE("MyNone", List(NamePT("Int"))),PackPE(List()), true)
    compile("MySome:MyNone:Int()") shouldEqual
      FunctionCallPE(LookupPE("MySome", List(CallPT(NamePT("MyNone"),List(NamePT("Int"))))),PackPE(List()), true)
  }

  // debt: fix
//  test("Array index") {
//    compile("board.(i)") shouldEqual DotCallPE(LookupPE("board", List()),PackPE(List(LookupPE("i", List()))),true)
//    compile("this.board.(i)") shouldEqual
//      DotCallPE(DotPE(LookupPE("this", List()), "board", true),PackPE(List(LookupPE("i", List()))),true)
//  }
}
