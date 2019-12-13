package net.verdagon.vale.parser.patterns

import net.verdagon.vale.parser.Patterns._
import net.verdagon.vale.parser.VParser._
import net.verdagon.vale.parser._
import net.verdagon.vale.vfail
import org.scalatest.{FunSuite, Matchers}

class DestructureTests extends FunSuite with Matchers {
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

  test("Only empty destructure") {
    compile("[]") shouldEqual
        withDestructure()
  }
  test("Only two-element destructure") {
    compile("[a, b]") shouldEqual
        withDestructure(capture("a"), capture("b"))
  }
  test("Two-element destructure with ignore") {
    compile("[_, b]") shouldEqual
        PatternPP(
          None,None,
          Some(List(None, Some(capture("b")))),
          None)
  }
  test("Capture with destructure") {
    compile("a [a, b]") shouldEqual
        PatternPP(
          Some(CaptureP("a",FinalP)),
          None,
          Some(List(Some(capture("a")), Some(capture("b")))),
          None)
  }
  test("Capture with types inside") {
    compile("a [:Int, :Bool]") shouldEqual
        PatternPP(
          Some(CaptureP("a",FinalP)),
          None,
          Some(List(Some(fromEnv("Int")), Some(fromEnv("Bool")))),
          None)
  }
  test("Destructure with type inside") {
    compile("[a: Int, b: Bool]") shouldEqual
        withDestructure(
          capturedWithType("a", NamePPT("Int")),
          capturedWithType("b", NamePPT("Bool")))
  }
  test("Nested destructures A") {
    compile("[a, [b, c]]") shouldEqual
        withDestructure(
          capture("a"),
          withDestructure(
            capture("b"),
            capture("c")))
  }
  test("Nested destructures B") {
    compile("[[a], b]") shouldEqual
        withDestructure(
          withDestructure(
            capture("a")),
          capture("b"))
  }
  test("Nested destructures C") {
    compile("[[[a]]]") shouldEqual
        withDestructure(
          withDestructure(
            withDestructure(
              capture("a"))))
  }
}
