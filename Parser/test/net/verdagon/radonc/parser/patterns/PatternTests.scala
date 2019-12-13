package net.verdagon.radonc.parser.patterns

import net.verdagon.radonc.parser.Patterns._
import net.verdagon.radonc.parser.VParser._
import net.verdagon.radonc.parser._
import net.verdagon.radonc.vfail
import org.scalatest.{FunSuite, Matchers}

class PatternTests extends FunSuite with Matchers {
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

  test("Simple Int") {
    // Make sure every pattern on the way down to kind can match Int
    compile(typeIdentifier,"Int") shouldEqual "Int"
    compile(kindPattern,"Int") shouldEqual NamePPT("Int")
    compile(patternType,"Int") shouldEqual PatternTypePPI(None, None, Some(NamePPT("Int")))
    compile(atomPattern,":Int") shouldEqual Patterns.fromEnv("Int")
  }
  test("Pattern Templexes") {
    compile(patternType,"Int") shouldEqual PatternTypePPI(None, None, Some(NamePPT("Int")))
    compile(patternType,"*Int") shouldEqual PatternTypePPI(Some(ShareP), None, Some(NamePPT("Int")))
  }
  test("Name-only Capture") {
    compile(atomPattern,"a") shouldEqual
        PatternPP(Some(CaptureP("a", FinalP)), None, None,None)
  }
  test("Empty pattern list") {
    compile(patternPrototypeParams,"()") shouldEqual List()
  }
  test("Pattern list with only two captures") {
    val list = compile(patternPrototypeParams, "(a, b)")
    list shouldEqual
        List(capture("a"), capture("b"))
  }
  test("Simple pattern doesn't eat = after it") {
    compile(atomPattern, "a : Int")
    checkFail(atomPattern, "a : Int=")
    checkFail(atomPattern, "a : Int =")
    checkFail(atomPattern, "a : Int = m")
    checkFail(atomPattern, "a : Int = m;")
  }
  test("Empty pattern") {
    compile("_") shouldEqual PatternPP(None,None,None,None)
  }



  test("Capture with type with destructure") {
    compile("a: Moo[a, b]") shouldEqual
        PatternPP(
          Some(CaptureP("a",FinalP)),
          Some(NamePPT("Moo")),
          Some(List(Some(capture("a")),Some(capture("b")))),
          None)
  }


  test("CSTODTS") {
    // This tests us handling an ambiguity properly, see CSTODTS in docs.
    compile("moo: #T[a: Int]") shouldEqual
        PatternPP(
          Some(CaptureP("moo",FinalP)),
          Some(RunePPT("T")),
          Some(List(Some(PatternPP(Some(CaptureP("a",FinalP)),Some(NamePPT("Int")),None,None)))),
          None)
  }

  test("Capture with destructure with type outside") {
    compile("a: [Int, Bool][a, b]") shouldEqual
        PatternPP(
          Some(CaptureP("a",FinalP)),
          Some(
            ManualSequencePPT(
                  List(
                    NamePPT("Int"),
                    NamePPT("Bool")))),
          Some(List(Some(capture("a")), Some(capture("b")))),
          None)
  }

  test("Virtual function") {
    compile(VParser.atomPattern, "this: virtual Car") shouldEqual
      PatternPP(Some(CaptureP("this",FinalP)),Some(NamePPT("Car")),None,Some(AbstractP))
  }
}
