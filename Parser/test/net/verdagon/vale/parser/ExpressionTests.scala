package net.verdagon.vale.parser

import net.verdagon.vale.vassert
import org.scalatest.{FunSuite, Matchers}

class ExpressionTests extends FunSuite with Matchers with Collector {
  private def compile[T](parser: VParser.Parser[IExpressionPE], code: String): IExpressionPE = {
    VParser.parse(parser, code.toCharArray()) match {
      case VParser.NoSuccess(msg, input) => {
        fail();
      }
      case VParser.Success(expr, rest) => {
        vassert(
          rest.atEnd,
          "Parsed \"" + code.slice(0, rest.offset) + "\" as \"" + expr + "\" but stopped at \"" + code.slice(rest.offset, code.length) + "\"")
        expr
      }
    }
  }
  private def compile(code: String): IExpressionPE = {
    compile(VParser.expression, code)
  }

  test("PE") {
    compile("4") shouldHave { case IntLiteralPE(_, 4) => }
  }

  test("2") {
    compile("4 + 5") shouldHave { case FunctionCallPE(_,LookupPE(StringP(_, "+"), List()), List(IntLiteralPE(_, 4), IntLiteralPE(_, 5)),true) => }
  }

  test("Floats") {
    compile("4.2") shouldHave { case FloatLiteralPE(_, 4.2f) => }
  }

  test("4") {
    compile("+(4, 5)") shouldHave { case FunctionCallPE(_,LookupPE(StringP(_, "+"), List()), List(IntLiteralPE(_, 4), IntLiteralPE(_, 5)),true) => }
  }

  test("5") {
    compile("x(y)") shouldHave { case FunctionCallPE(_,LookupPE(StringP(_, "x"), List()), List(LookupPE(StringP(_, "y"), List())),true) => }
  }

  test("6") {
    compile("not y") shouldHave { case FunctionCallPE(_,LookupPE(StringP(_, "not"), List()), List(LookupPE(StringP(_, "y"), List())),true) => }
  }

  test("Lending result of function call") {
    compile("&Muta()") shouldHave { case LendPE(FunctionCallPE(_,LookupPE(StringP(_, "Muta"), List()), List(),true)) => }
  }

  test("Method call") {
    compile("x.shout()") shouldHave {
      case MethodCallPE(
        _,
        LookupPE(StringP(_,"x"),List()),
        true,
        LookupPE(StringP(_,"shout"),List()),List()) =>
    }
  }

  test("Moving method call") {
    compile("x^.shout()") shouldHave {
      case MethodCallPE(_,LookupPE(StringP(_,"x"),List()),false,LookupPE(StringP(_,"shout"),List()),List()) =>
    }
  }

  test("Templated function call") {
    compile("toArray<imm>(&result)") shouldHave {
      case FunctionCallPE(_,
        LookupPE(StringP(_, "toArray"),List(MutabilityPT(ImmutableP))),
        List(LendPE(LookupPE(StringP(_, "result"),List()))),
        true) =>
    }
  }

  test("Templated method call") {
    compile("result.toArray<imm>()") shouldHave {
      case MethodCallPE(_,LookupPE(StringP(_,"result"),List()),true,LookupPE(StringP(_,"toArray"),List(MutabilityPT(ImmutableP))),List()) =>
    }
  }

  test("Custom binaries") {
    compile("not y florgle not x") shouldHave {
      case FunctionCallPE(_,
          LookupPE(StringP(_, "florgle"), List()),
          List(
            FunctionCallPE(_,
              LookupPE(StringP(_, "not"), List()),
              List(LookupPE(StringP(_, "y"), List())),
              true),
            FunctionCallPE(_,
              LookupPE(StringP(_, "not"), List()),
              List(LookupPE(StringP(_, "x"), List())),
              true)),
          true) =>
    }
  }

  test("Custom with noncustom binaries") {
    compile("a + b florgle x * y") shouldHave {
      case FunctionCallPE(_,
          LookupPE(StringP(_, "florgle"), List()),
          List(
            FunctionCallPE(_,
              LookupPE(StringP(_, "+"), List()),
              List(LookupPE(StringP(_, "a"), List()), LookupPE(StringP(_, "b"), List())),
              true),
            FunctionCallPE(_,
              LookupPE(StringP(_, "*"), List()),
              List(LookupPE(StringP(_, "x"), List()), LookupPE(StringP(_, "y"), List())),
              true)),
          true) =>
    }
  }

  test("Template calling") {
    compile("MyNone<Int>()") shouldHave {
      case FunctionCallPE(_,LookupPE(StringP(_, "MyNone"), List(NameOrRunePT(StringP(_, "Int")))),List(), true) =>
    }
    compile("MySome<MyNone<Int>>()") shouldHave {
      case FunctionCallPE(_,LookupPE(StringP(_, "MySome"), List(CallPT(NameOrRunePT(StringP(_, "MyNone")),List(NameOrRunePT(StringP(_, "Int")))))),List(), true) =>
    }
  }

  test(">=") {
    // It turns out, this was only parsing "9 >=" because it was looking for > specifically (in fact, it was looking
    // for + - * / < >) so it parsed as >(9, =) which was bad. We changed the infix operator parser to expect the
    // whitespace on both sides, so that it was forced to parse the entire thing.
    compile(VParser.expression,"9 >= 3") shouldHave {
      case FunctionCallPE(_,LookupPE(StringP(_, ">="),List()),List(IntLiteralPE(_, 9), IntLiteralPE(_, 3)),true) =>
    }
  }

  test("Indexing") {
    compile(VParser.expression,"arr[4]") shouldHave {
      case DotCallPE(_,LookupPE(StringP(_,arr),List()),List(IntLiteralPE(_,4))) =>
    }
  }


  // debt: fix
//  test("Array index") {
//    compile("board.(i)") shouldEqual DotCallPE(LookupPE(StringP(_, "board"), List()),PackPE(List(LookupPE(StringP(_, "i"), List()))),true)
//    compile("this.board.(i)") shouldEqual
//      DotCallPE(DotPE(_,LookupPE(StringP(_, "this"), List()), "board", true),PackPE(List(LookupPE(StringP(_, "i"), List()))),true)
//  }
}
