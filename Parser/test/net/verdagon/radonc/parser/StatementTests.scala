package net.verdagon.radonc.parser

import org.scalatest.{FunSuite, Matchers}

class StatementTests extends FunSuite with Matchers {
  private def compile(code: String): IExpressionPE = {
    VParser.parse(VParser.statement, code.toCharArray()) match {
      case VParser.NoSuccess(msg, input) => {
        fail(msg);
      }
      case VParser.Success(expr, rest) => {
        expr
      }
    }
  }

  test("Simple let") {
    compile("x = 4;") shouldEqual
        LetPE(List(),PatternPP(Some(CaptureP("x",FinalP)),None,None,None),IntLiteralPE(4))
  }

  test("8") {
    compile("[x, y] = (4, 5);") shouldEqual
        LetPE(
          List(),
          PatternPP(
            None,
            None,
            Some(
              List(
                Some(PatternPP(Some(CaptureP("x",FinalP)),None,None,None)),
                Some(PatternPP(Some(CaptureP("y",FinalP)),None,None,None)))),
            None),
          PackPE(List(IntLiteralPE(4), IntLiteralPE(5))))
  }

  test("9") {
    compile("mut x.a = 5;") shouldEqual MutatePE(DotPE(LookupPE("x", List()), LookupPE("a", List()), true), IntLiteralPE(5));
  }

  test("1PE") {
    compile("""mut board.PE.PE.symbol = "v";""") shouldEqual
        MutatePE(DotPE(DotPE(DotPE(LookupPE("board", List()), LookupPE("PE", List()), true), LookupPE("PE", List()), true), LookupPE("symbol", List()), true), StrLiteralPE("v"));
  }

  test("Test simple let") {
    compile("x = 3;") shouldEqual
      LetPE(List(),PatternPP(Some(CaptureP("x",FinalP)),None,None,None),IntLiteralPE(3))
  }

  test("Test simple mut") {
    compile("mut x = 5;") shouldEqual
      MutatePE(LookupPE("x", List()),IntLiteralPE(5))
  }

  test("Test swap") {
    compile("exch x, y;") shouldEqual
      SwapPE(LookupPE("x",List()),LookupPE("y",List()))
  }

  test("Dot on function call's result") {
    compile("Wizard(8).charges;") shouldEqual
        DotPE(
          FunctionCallPE(
            LookupPE("Wizard", List()),
            PackPE(List(IntLiteralPE(8))),
            true),
          LookupPE("charges", List()),
          true)
  }

  test("Let with pattern with only a capture") {
    compile("a = m;") shouldEqual
        LetPE(List(),Patterns.capture("a"),LookupPE("m", List()))
  }

  test("Let with simple pattern") {
    compile("a : Moo = m;") shouldEqual
        LetPE(
          List(),
          PatternPP(Some(CaptureP("a",FinalP)),Some(NamePPT("Moo")),None,None),
          LookupPE("m", List()))
  }

  test("Let with simple pattern in seq") {
    compile("[a : Moo] = m;") shouldEqual
        LetPE(
          List(),
          PatternPP(
            None,
            None,
            Some(List(Some(PatternPP(Some(CaptureP("a",FinalP)),Some(NamePPT("Moo")),None,None)))),
            None),
          LookupPE("m", List()))
  }

  test("Let with destructuring pattern") {
    compile(":Muta[] = m;") shouldEqual
      LetPE(List(),PatternPP(None,Some(NamePPT("Muta")),Some(List()),None),LookupPE("m", List()))
  }

  test("Ret") {
    compile("ret 3;") shouldEqual ReturnPE(IntLiteralPE(3))
  }
}
