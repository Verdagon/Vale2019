package net.verdagon.radonc.parser.patterns

import net.verdagon.radonc.parser.Patterns._
import net.verdagon.radonc.parser.VParser._
import net.verdagon.radonc.parser._
import net.verdagon.radonc.vfail
import org.scalatest.{FunSuite, Matchers}

class CaptureAndDestructureTests extends FunSuite with Matchers {
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


  test("Capture with destructure with type inside") {
    compile("a [a :Int, b :Bool]") shouldEqual
        PatternPP(
          Some(CaptureP("a",FinalP)),
          None,
          Some(
            List(
              Some(capturedWithType("a", NamePPT("Int"))),
              Some(capturedWithType("b", NamePPT("Bool"))))),
          None)
  }
  test("capture with empty sequence type") {
    compile("a: []") shouldEqual
        capturedWithType("a", ManualSequencePPT(List()))
  }
  test("capture with empty destructure") {
    compile("a []") shouldEqual
        PatternPP(Some(CaptureP("a",FinalP)),None,Some(List()),None)
  }
  test("Destructure with nested atom") {
    compile("a [ b : Int ]") shouldEqual
        PatternPP(
          Some(CaptureP("a", FinalP)),
          None,
          Some(
            List(Some(capturedWithType("b", NamePPT("Int"))))),
          None)
  }
}
