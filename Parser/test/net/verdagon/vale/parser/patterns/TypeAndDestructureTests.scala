package net.verdagon.vale.parser.patterns

import net.verdagon.vale.parser.Patterns._
import net.verdagon.vale.parser.VParser._
import net.verdagon.vale.parser._
import net.verdagon.vale.vfail
import org.scalatest.{FunSuite, Matchers}

class TypeAndDestructureTests extends FunSuite with Matchers with Collector {
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
    compile("_ Muta()") shouldHave {
      case PatternPP(_,
          None,
          Some(NameOrRunePPT(StringP(_, "Muta"))),
          Some(List()),
          None) =>
    }
  }

  test("Templated destructure") {
    compile("_ Muta<Int>()") shouldHave {
      case PatternPP(_,
          None,
          Some(
            CallPPT(
              NameOrRunePPT(StringP(_, "Muta")),
              List(NameOrRunePPT(StringP(_, "Int"))))),
          Some(List()),
          None) =>
    }
    compile("_ Muta<R>()") shouldHave {
        case PatternPP(_,
          None,
          Some(
            CallPPT(
              NameOrRunePPT(StringP(_, "Muta")),
              List(NameOrRunePPT(StringP(_, "R"))))),
          Some(List()),
          None) =>
    }
  }


  test("Destructure with type outside") {
    compile("_ [Int, Bool](a, b)") shouldHave {
      case PatternPP(_,
          None,
          Some(
            ManualSequencePPT(
                List(
                  NameOrRunePPT(StringP(_, "Int")),
                  NameOrRunePPT(StringP(_, "Bool"))))),
          Some(List(capture("a"), capture("b"))),
          None) =>
    }
  }
  test("Destructure with typeless capture") {
    compile("_ Muta(b)") shouldHave {
      case PatternPP(_,
          None,
          Some(NameOrRunePPT(StringP(_, "Muta"))),
          Some(List(PatternPP(_,Some(CaptureP(_,StringP(_, "b"),FinalP)),None,None,None))),
          None) =>
    }
  }
  test("Destructure with typed capture") {
    compile("_ Muta(b Marine)") shouldHave {
      case PatternPP(_,
          None,
          Some(NameOrRunePPT(StringP(_, "Muta"))),
          Some(List(PatternPP(_,Some(CaptureP(_,StringP(_, "b"),FinalP)),Some(NameOrRunePPT(StringP(_, "Marine"))),None,None))),
          None) =>
    }
  }
  test("Destructure with unnamed capture") {
    compile("_ Muta(_ Marine)") shouldHave {
      case PatternPP(_,
          None,
          Some(NameOrRunePPT(StringP(_, "Muta"))),
          Some(List(PatternPP(_,None,Some(NameOrRunePPT(StringP(_, "Marine"))),None,None))),
          None) =>
    }
  }
  test("Destructure with runed capture") {
    compile("_ Muta(_ R)") shouldHave {
      case PatternPP(_,
          None,
          Some(NameOrRunePPT(StringP(_, "Muta"))),
          Some(List(PatternPP(_,None,Some(NameOrRunePPT(StringP(_, "R"))),None,None))),
          None) =>
        }
  }
}
