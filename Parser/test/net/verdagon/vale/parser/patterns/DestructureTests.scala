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
        fail(msg);
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
    compile("()") shouldEqual
      withDestructure()
  }
  test("One element destructure") {
    compile("(a)") shouldEqual
      withDestructure(capture("a"))
  }
  test("One typed element destructure") {
    compile("( _ A )") shouldEqual
      withDestructure(withType(NameOrRunePPT("A")))
  }
  test("Only two-element destructure") {
    compile("(a, b)") shouldEqual
        withDestructure(capture("a"), capture("b"))
  }
  test("Two-element destructure with ignore") {
    compile("(_, b)") shouldEqual
        PatternPP(
          None,None,
          Some(List(Patterns.ignore(), capture("b"))),
          None)
  }
  test("Capture with destructure") {
    compile("a (x, y)") shouldEqual
      PatternPP(
        Some(CaptureP("a",FinalP)),
        None,
        Some(List(capture("x"), capture("y"))),
        None)
  }
  test("Type with destructure") {
    compile("A(a, b)") shouldEqual
      PatternPP(
        None,
        Some(NameOrRunePPT("A")),
        Some(List(capture("a"), capture("b"))),
        None)
  }
  test("Capture and type with destructure") {
    compile("a A(x, y)") shouldEqual
      PatternPP(
        Some(CaptureP("a",FinalP)),
        Some(NameOrRunePPT("A")),
        Some(List(capture("x"), capture("y"))),
        None)
  }
  test("Capture with types inside") {
    compile("a (_ Int, _ Bool)") shouldEqual
        PatternPP(
          Some(CaptureP("a",FinalP)),
          None,
          Some(List(fromEnv("Int"), fromEnv("Bool"))),
          None)
  }
  test("Destructure with type inside") {
    compile("(a Int, b Bool)") shouldEqual
        withDestructure(
          capturedWithType("a", NameOrRunePPT("Int")),
          capturedWithType("b", NameOrRunePPT("Bool")))
  }
  test("Nested destructures A") {
    compile("(a, (b, c))") shouldEqual
        withDestructure(
          capture("a"),
          withDestructure(
            capture("b"),
            capture("c")))
  }
  test("Nested destructures B") {
    compile("((a), b)") shouldEqual
        withDestructure(
          withDestructure(
            capture("a")),
          capture("b"))
  }
  test("Nested destructures C") {
    compile("(((a)))") shouldEqual
        withDestructure(
          withDestructure(
            withDestructure(
              capture("a"))))
  }
}
