package net.verdagon.vale.parser.patterns

import net.verdagon.vale.parser.Patterns._
import net.verdagon.vale.parser.VParser._
import net.verdagon.vale.parser._
import net.verdagon.vale.vfail
import org.scalatest.{FunSuite, Matchers}

class TypeAndDestructureTests extends FunSuite with Matchers {
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




  test("Empty destructure") {
    compile("_ Muta()") shouldEqual
        PatternPP(
          None,
          Some(NameOrRunePPT("Muta")),
          Some(List()),
          None)
  }

  test("Templated destructure") {
    compile("_ Muta<Int>()") shouldEqual
        PatternPP(
          None,
          Some(
            CallPPT(
              NameOrRunePPT("Muta"),
              List(NameOrRunePPT("Int")))),
          Some(List()),
          None)
    compile("_ Muta<R>()") shouldEqual
        PatternPP(
          None,
          Some(
            CallPPT(
              NameOrRunePPT("Muta"),
              List(NameOrRunePPT("R")))),
          Some(List()),
          None)
  }


  test("Destructure with type outside") {
    compile("_ [Int, Bool](a, b)") shouldEqual
        PatternPP(
          None,
          Some(
            ManualSequencePPT(
                List(
                  NameOrRunePPT("Int"),
                  NameOrRunePPT("Bool")))),
          Some(List(capture("a"), capture("b"))),
          None)
  }
  test("Destructure with typeless capture") {
    compile("_ Muta(b)") shouldEqual
        PatternPP(
          None,
          Some(NameOrRunePPT("Muta")),
          Some(List(PatternPP(Some(CaptureP("b",FinalP)),None,None,None))),
          None)
  }
  test("Destructure with typed capture") {
    compile("_ Muta(b Marine)") shouldEqual
        PatternPP(
          None,
          Some(NameOrRunePPT("Muta")),
          Some(List(PatternPP(Some(CaptureP("b",FinalP)),Some(NameOrRunePPT("Marine")),None,None))),
          None)
  }
  test("Destructure with unnamed capture") {
    compile("_ Muta(_ Marine)") shouldEqual
        PatternPP(
          None,
          Some(NameOrRunePPT("Muta")),
          Some(List(PatternPP(None,Some(NameOrRunePPT("Marine")),None,None))),
          None)
  }
  test("Destructure with runed capture") {
    compile("_ Muta(_ R)") shouldEqual
        PatternPP(
          None,
          Some(NameOrRunePPT("Muta")),
          Some(List(PatternPP(None,Some(NameOrRunePPT("R")),None,None))),
          None)
  }
}
