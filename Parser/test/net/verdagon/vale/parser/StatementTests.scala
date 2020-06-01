package net.verdagon.vale.parser

import net.verdagon.vale.vimpl
import org.scalatest.{FunSuite, Matchers}

class StatementTests extends FunSuite with Matchers with Collector {
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
    compile("x = 4;") shouldHave {
      case LetPE(List(), PatternPP(Some(CaptureP(StringP(_, "x"), FinalP)), None, None, None), IntLiteralPE(4)) =>
    }
  }

  test("8") {
    compile("(x, y) = [4, 5];") shouldHave {
      case LetPE(
          List(),
          PatternPP(
            None,
            None,
            Some(
              List(
                PatternPP(Some(CaptureP(StringP(_, "x"),FinalP)),None,None,None),
                PatternPP(Some(CaptureP(StringP(_, "y"),FinalP)),None,None,None))),
            None),
          SequencePE(List(IntLiteralPE(4), IntLiteralPE(5)))) =>
    }
  }

  test("9") {
    compile("mut x.a = 5;") shouldHave {
      case MutatePE(DotPE(LookupPE(StringP(_, "x"), List()), LookupPE(StringP(_, "a"), List()), true), IntLiteralPE(5)) =>
    }
  }

  test("1PE") {
    compile("""mut board.PE.PE.symbol = "v";""") shouldHave {
      case MutatePE(DotPE(DotPE(DotPE(LookupPE(StringP(_, "board"), List()), LookupPE(StringP(_, "PE"), List()), true), LookupPE(StringP(_, "PE"), List()), true), LookupPE(StringP(_, "symbol"), List()), true), StrLiteralPE(StringP(_, "v"))) =>
    }
  }

  test("Test simple let") {
    compile("x = 3;") shouldHave {
      case LetPE(List(),PatternPP(Some(CaptureP(StringP(_, "x"),FinalP)),None,None,None),IntLiteralPE(3)) =>
    }
  }

  test("Test varying let") {
    compile("x! = 3;") shouldHave {
      case LetPE(List(),PatternPP(Some(CaptureP(StringP(_, "x"),VaryingP)),None,None,None),IntLiteralPE(3)) =>
    }
  }

  test("Test simple mut") {
    compile("mut x = 5;") shouldHave {
      case MutatePE(LookupPE(StringP(_, "x"), List()),IntLiteralPE(5)) =>
    }
  }

  test("Test swap") {
    compile("exch x, y;") shouldHave {
      case SwapPE(LookupPE(StringP(_, "x"),List()),LookupPE(StringP(_, "y"),List())) =>
    }
  }

  test("Dot on function call's result") {
    compile("Wizard(8).charges;") shouldHave {
      case DotPE(
          FunctionCallPE(
            LookupPE(StringP(_, "Wizard"), List()),
            List(IntLiteralPE(8)),
            true),
          LookupPE(StringP(_, "charges"), List()),
          true) =>
    }
  }

  test("Let with pattern with only a capture") {
    compile("a = m;") shouldHave {
      case LetPE(List(),Patterns.capture("a"),LookupPE(StringP(_, "m"), List())) =>
    }
  }

  test("Let with simple pattern") {
    compile("a Moo = m;") shouldHave {
      case LetPE(
          List(),
          PatternPP(Some(CaptureP(StringP(_, "a"),FinalP)),Some(NameOrRunePPT(StringP(_, "Moo"))),None,None),
          LookupPE(StringP(_, "m"), List())) =>
    }
  }

  test("Let with simple pattern in seq") {
    compile("(a Moo) = m;") shouldHave {
      case LetPE(
          List(),
          PatternPP(
            None,
            None,
            Some(List(PatternPP(Some(CaptureP(StringP(_, "a"),FinalP)),Some(NameOrRunePPT(StringP(_, "Moo"))),None,None))),
            None),
          LookupPE(StringP(_, "m"), List())) =>
    }
  }

  test("Let with destructuring pattern") {
    compile("Muta() = m;") shouldHave {
      case LetPE(List(),PatternPP(None,Some(NameOrRunePPT(StringP(_, "Muta"))),Some(List()),None),LookupPE(StringP(_, "m"), List())) =>
    }
  }

  test("Ret") {
    compile("ret 3;") shouldHave {
      case ReturnPE(IntLiteralPE(3)) =>
    }
  }


  test("eachI") {
    compile("eachI row (cellI, cell){ 0 }") shouldHave {
      case FunctionCallPE(
        LookupPE(StringP(_, "eachI"),List()),
        List(
          LookupPE(StringP(_, "row"),List()),
          LambdaPE(
            FunctionP(
              _,None,None,None,None,None,
              Some(ParamsP(_, List(PatternPP(Some(CaptureP(StringP(_, "cellI"),FinalP)),None,None,None), PatternPP(Some(CaptureP(StringP(_, "cell"),FinalP)),None,None,None)))),
              None,
              Some(BlockPE(List(IntLiteralPE(0))))))),
        true) =>
    }
  }

  test("eachI with borrow") {
    compile("eachI &row (cellI, cell){ 0 }") shouldHave {
      case FunctionCallPE(
        LookupPE(StringP(_, "eachI"),List()),
        List(
          LendPE(LookupPE(StringP(_, "row"),List())),
          LambdaPE(
            FunctionP(
              _,None,None,None,None,None,
              Some(ParamsP(_, List(PatternPP(Some(CaptureP(StringP(_, "cellI"),FinalP)),None,None,None), PatternPP(Some(CaptureP(StringP(_, "cell"),FinalP)),None,None,None)))),
              None,
              Some(BlockPE(List(IntLiteralPE(0))))))),
        true) =>
    }
  }
}
