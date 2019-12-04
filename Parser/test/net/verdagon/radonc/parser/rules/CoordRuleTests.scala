package net.verdagon.radonc.parser.rules

import net.verdagon.radonc.parser.VParser._
import net.verdagon.radonc.parser._
import net.verdagon.radonc.vfail
import org.scalatest.{FunSuite, Matchers}

class CoordRuleTests extends FunSuite with Matchers {
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
    VParser.parse(parser, "") match {
      case VParser.NoSuccess(_, _) =>
      case VParser.Success(_, rest) => {
        if (!rest.atEnd) {
          fail(rest.pos.longString)
        }
        fail()
      }
    }
  }

  test("Empty Coord rule") {
    compile(rulePR, ":Ref") shouldEqual TypedPR(None,CoordTypePR)
  }

  test("Coord with rune") {
    compile(rulePR, "#T: Ref") shouldEqual TypedPR(Some("T"),CoordTypePR)
  }

  test("Coord with destructure only") {
    compile(rulePR, ":Ref[_, _]") shouldEqual
        ComponentsPR(TypedPR(None,CoordTypePR),List(TemplexPR(AnonymousRunePRT()), TemplexPR(AnonymousRunePRT())))
  }

  test("Coord with rune and destructure") {
    compile(rulePR, "#T: Ref[_, _]") shouldEqual
        ComponentsPR(TypedPR(Some("T"),CoordTypePR),List(TemplexPR(AnonymousRunePRT()), TemplexPR(AnonymousRunePRT())))
    compile(rulePR, "#T: Ref[own, _]") shouldEqual
        ComponentsPR(
          TypedPR(Some("T"),CoordTypePR),
          List(TemplexPR(OwnershipPRT(OwnP)), TemplexPR(AnonymousRunePRT())))
  }

  test("Coord matches plain Int") {
    // Coord can do this because I want to be able to say:
    //   fn moo
    //   rules(#T = (Int):Void)
    //   (a: #T)
    // instead of:
    //   fn moo
    //   rules(
    //     Ref#T[_, _, Ref[_, _, Int]]:Ref[_, _, Void]))
    //   (a: #T)
    compile(rulePR, "Int") shouldEqual TemplexPR(NamePRT("Int"))
//        CoordPR(None,None,None,None,None,Some(List(NameTemplexPR("Int"))))

  }

  test("Coord with Int in kind rule") {
    compile(rulePR, "#T: Ref[_, Int]") shouldEqual
        ComponentsPR(
          TypedPR(Some("T"),CoordTypePR),
          List(TemplexPR(AnonymousRunePRT()), TemplexPR(NamePRT("Int"))))
//      runedTCoordWithEnvKind("T", "Int")

  }

  test("Coord with specific Kind rule") {
    compile(rulePR, "#T: Ref[_, :Kind[mut]]") shouldEqual
        ComponentsPR(
          TypedPR(Some("T"),CoordTypePR),
          List(
            TemplexPR(AnonymousRunePRT()),
            ComponentsPR(
              TypedPR(None,KindTypePR),List(TemplexPR(MutabilityPRT(MutableP))))))
  }

  test("Coord with value") {
    compile(rulePR, "#T: Ref = Int") shouldEqual
        EqualsPR(
          TypedPR(Some("T"),CoordTypePR),
          TemplexPR(NamePRT("Int")))
  }

  test("Coord with destructure and value") {
    compile(rulePR, "#T: Ref[_, _] = Int") shouldEqual
        EqualsPR(
          ComponentsPR(TypedPR(Some("T"),CoordTypePR),List(TemplexPR(AnonymousRunePRT()), TemplexPR(AnonymousRunePRT()))),
          TemplexPR(NamePRT("Int")))
//        runedTCoordWithValue("T", NameTemplexPR("Int"))
  }

  test("Coord with sequence in value spot") {
    compile(rulePR, "#T: Ref = [Int, Bool]") shouldEqual
        EqualsPR(
          TypedPR(Some("T"),CoordTypePR),
          TemplexPR(
            ManualSequencePRT(
              List(NamePRT("Int"), NamePRT("Bool")))))
  }

  test("Braces without Ref is sequence") {
    compile(rulePR, "[Int, Bool]") shouldEqual
        TemplexPR(
          ManualSequencePRT(
            List(NamePRT("Int"), NamePRT("Bool"))))
  }
}
