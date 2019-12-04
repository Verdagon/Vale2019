package net.verdagon.radonc.parser.patterns

import net.verdagon.radonc.parser.Patterns._
import net.verdagon.radonc.parser.VParser._
import net.verdagon.radonc.parser._
import net.verdagon.radonc.vfail
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
    compile(":Muta[]") shouldEqual
        PatternPP(
          None,
          Some(NamePPT("Muta")),
          Some(List()),
          None)
  }

  test("Templated destructure") {
    compile(":Muta:Int[]") shouldEqual
        PatternPP(
          None,
          Some(
            CallPPT(
              NamePPT("Muta"),
              List(NamePPT("Int")))),
          Some(List()),
          None)
    compile(":Muta:#R[]") shouldEqual
        PatternPP(
          None,
          Some(
            CallPPT(
              NamePPT("Muta"),
              List(RunePPT("R")))),
          Some(List()),
          None)
  }


  test("Destructure with type outside") {
    compile(":[Int, Bool][a, b]") shouldEqual
        PatternPP(
          None,
          Some(
            ManualSequencePPT(
                List(
                  NamePPT("Int"),
                  NamePPT("Bool")))),
          Some(List(Some(capture("a")), Some(capture("b")))),
          None)
  }
  test("Destructure with typeless capture") {
    compile(":Muta[b]") shouldEqual
        PatternPP(
          None,
          Some(NamePPT("Muta")),
          Some(List(Some(PatternPP(Some(CaptureP("b",FinalP)),None,None,None)))),
          None)
  }
  test("Destructure with typed capture") {
    compile(":Muta[b: Marine]") shouldEqual
        PatternPP(
          None,
          Some(NamePPT("Muta")),
          Some(List(Some(PatternPP(Some(CaptureP("b",FinalP)),Some(NamePPT("Marine")),None,None)))),
          None)
  }
  test("Destructure with unnamed capture") {
    compile(":Muta[:Marine]") shouldEqual
        PatternPP(
          None,
          Some(NamePPT("Muta")),
          Some(List(Some(PatternPP(None,Some(NamePPT("Marine")),None,None)))),
          None)
  }
  test("Destructure with runed capture") {
    compile(":Muta[:#R]") shouldEqual
        PatternPP(
          None,
          Some(NamePPT("Muta")),
          Some(List(Some(PatternPP(None,Some(RunePPT("R")),None,None)))),
          None)
  }
}
