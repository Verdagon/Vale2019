package net.verdagon.radonc.parser.patterns

import net.verdagon.radonc.{parser, vfail}
import net.verdagon.radonc.parser.Patterns.{fromEnv, withType}
import net.verdagon.radonc.parser.VParser._
import net.verdagon.radonc.parser._
import org.scalatest.{FunSuite, Matchers}

class TypeTests extends FunSuite with Matchers {
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
    compile(": Int") shouldEqual fromEnv("Int")
    compile("_ : Int") shouldEqual fromEnv("Int")
  }
  test("Callable type") {
    compile(":fn(#T)Void") shouldEqual
        withType(
          FunctionPPT(
            None,
            List(RunePPT("T")),
            NamePPT("Void")))
  }
  test("15a") {
    compile(":[3 * MutableStruct]") shouldEqual
        withType(
          RepeaterSequencePPT(
              MutabilityPPT(MutableP),
              IntPPT(3),
              NamePPT("MutableStruct")))
  }

  test("15b") {
    compile(":[:imm 3 * MutableStruct]") shouldEqual
      withType(
        RepeaterSequencePPT(
          MutabilityPPT(ImmutableP),
          IntPPT(3),
          NamePPT("MutableStruct")))
  }

  test("Sequence type") {
    compile(":[Int, Bool]") shouldEqual
        withType(
          ManualSequencePPT(
            List(
              NamePPT("Int"),
              NamePPT("Bool"))))
  }
  test("15") {
    compile(":&[3 * MutableStruct]") shouldEqual
      PatternPP(
        None,
        Some(
          OwnershippedPPT(
            BorrowP,
            RepeaterSequencePPT(
              MutabilityPPT(MutableP),
              IntPPT(3),
              NamePPT("MutableStruct")))),
        None,
        None)
  }
  test("15z") {
    compile(": MyOption:MyList:Int") shouldEqual
      PatternPP(
        None,
        Some(
          CallPPT(
            NamePPT("MyOption"),
            List(
              CallPPT(
                NamePPT("MyList"),
                List(
                  NamePPT("Int")))))),
        None,
        None)
  }
}
