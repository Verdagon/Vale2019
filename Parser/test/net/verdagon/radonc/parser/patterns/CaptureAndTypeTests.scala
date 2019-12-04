package net.verdagon.radonc.parser.patterns

import net.verdagon.radonc.parser.Patterns.{capturedWithType, capturedWithTypeRune}
import net.verdagon.radonc.parser.VParser._
import net.verdagon.radonc.parser._
import net.verdagon.radonc.vfail
import org.scalatest.{FunSuite, Matchers}

class CaptureAndTypeTests extends FunSuite with Matchers {
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

  test("Capture with type") {
    compile("a : Int") shouldEqual
        capturedWithType("a", NamePPT("Int"))
  }
  test("Simple capture with tame") {
    compile("a : #T") shouldEqual capturedWithTypeRune("a","T")
  }
  test("Capture with borrow tame") {
    compile("arr: &#R") shouldEqual
        PatternPP(
          Some(CaptureP("arr",FinalP)),
          Some(OwnershippedPPT(BorrowP, RunePPT("R"))),
          None,
          None)
  }
}
