package net.verdagon.vale.parser.patterns

import net.verdagon.vale.{parser, vfail}
import net.verdagon.vale.parser.Patterns.{fromEnv, withType}
import net.verdagon.vale.parser.VParser._
import net.verdagon.vale.parser._
import org.scalatest.{FunSuite, Matchers}

class TypeTests extends FunSuite with Matchers {
  private def compile[T](parser: VParser.Parser[T], code: String): T = {
    VParser.parse(parser, code.toCharArray()) match {
      case VParser.NoSuccess(msg, input) => {
        fail(msg + "\n" + input);
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
    VParser.parse(parser, code) match {
      case VParser.NoSuccess(_, _) =>
      case VParser.Success(_, rest) => {
        if (!rest.atEnd) {
          // That's good, it didn't parse all of it
        } else {
          fail()
        }
      }
    }
  }

  test("Ignoring name") {
    compile("_ Int") shouldEqual fromEnv("Int")
  }
  test("Callable type") {
    compile("_ fn(T)Void") shouldEqual
        withType(
          FunctionPPT(
            None,
            List(NameOrRunePPT("T")),
            NameOrRunePPT("Void")))
  }
  test("15a") {
    compile("_ [3 * MutableStruct]") shouldEqual
        withType(
          RepeaterSequencePPT(
              MutabilityPPT(MutableP),
              IntPPT(3),
              NameOrRunePPT("MutableStruct")))
  }

  test("15b") {
    compile("_ [<imm> 3 * MutableStruct]") shouldEqual
      withType(
        RepeaterSequencePPT(
          MutabilityPPT(ImmutableP),
          IntPPT(3),
          NameOrRunePPT("MutableStruct")))
  }

  test("Sequence type") {
    compile("_ [Int, Bool]") shouldEqual
        withType(
          ManualSequencePPT(
            List(
              NameOrRunePPT("Int"),
              NameOrRunePPT("Bool"))))
  }
  test("15") {
    compile("_ &[3 * MutableStruct]") shouldEqual
      PatternPP(
        None,
        Some(
          OwnershippedPPT(
            BorrowP,
            RepeaterSequencePPT(
              MutabilityPPT(MutableP),
              IntPPT(3),
              NameOrRunePPT("MutableStruct")))),
        None,
        None)
  }
  test("15z") {
    compile("_ MyOption<MyList<Int>>") shouldEqual
      PatternPP(
        None,
        Some(
          CallPPT(
            NameOrRunePPT("MyOption"),
            List(
              CallPPT(
                NameOrRunePPT("MyList"),
                List(
                  NameOrRunePPT("Int")))))),
        None,
        None)
  }
}
