package net.verdagon.vale.parser.rules

import net.verdagon.vale.parser.VParser._
import net.verdagon.vale.parser._
import net.verdagon.vale.vfail
import org.scalatest.{FunSuite, Matchers}

class KindRuleTests extends FunSuite with Matchers {
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

  test("Empty Kind rule") {
    compile(rulePR, "_ Kind") shouldEqual TypedPR(None,KindTypePR)
  }

  test("Kind with rune") {
    compile(rulePR, "T Kind") shouldEqual TypedPR(Some("T"),KindTypePR)
    //runedTKind("T")
  }

  test("Kind with destructure only") {
    compile(rulePR, "Kind(_)") shouldEqual
        ComponentsPR(TypedPR(None,KindTypePR),List(TemplexPR(AnonymousRunePRT())))
//        KindPR(None, KindTypePR, None, None)
  }

  test("Kind with rune and destructure") {
    compile(rulePR, "T Kind(_)") shouldEqual
        ComponentsPR(TypedPR(Some("T"),KindTypePR),List(TemplexPR(AnonymousRunePRT())))
    compile(rulePR, "T Kind(mut)") shouldEqual
        ComponentsPR(
          TypedPR(Some("T"),KindTypePR),
          List(TemplexPR(MutabilityPRT(MutableP))))
  }

  test("Kind matches plain Int") {
    compile(rulePR, "Int") shouldEqual
        TemplexPR(NameOrRunePRT("Int"))
  }

  test("Kind with value") {
    compile(rulePR, "T Kind = Int") shouldEqual
        EqualsPR(TypedPR(Some("T"),KindTypePR),TemplexPR(NameOrRunePRT("Int")))
  }

  test("Kind with destructure and value") {
    compile(rulePR, "T Kind(_) = Int") shouldEqual
        EqualsPR(
          ComponentsPR(TypedPR(Some("T"),KindTypePR),List(TemplexPR(AnonymousRunePRT()))),
          TemplexPR(NameOrRunePRT("Int")))
  }

  test("Kind with sequence in value spot") {
    compile(rulePR, "T Kind = [Int, Bool]") shouldEqual
        EqualsPR(
          TypedPR(Some("T"),KindTypePR),
          TemplexPR(
            ManualSequencePRT(
              List(NameOrRunePRT("Int"), NameOrRunePRT("Bool")))))
  }

  test("Braces without Kind is sequence") {
    compile(rulePR, "[Int, Bool]") shouldEqual
        TemplexPR(
          ManualSequencePRT(
            List(NameOrRunePRT("Int"), NameOrRunePRT("Bool"))))
  }

//  test("Simple kind filters") {
//    compile(rulePR, ":Struct") shouldEqual
//        TypedPR(None,StructTypePR)
//    compile(rulePR, ":Interface") shouldEqual
//        TypedPR(None,InterfaceTypePR)
////    compile(rulePR, ":Callable") shouldEqual
////        TypedPR(None,CallableTypePR)
//    compile(rulePR, ":KindTemplate") shouldEqual
//        TypedPR(None,CitizenTemplateTypePR)
//    compile(rulePR, ":Seq") shouldEqual
//        TypedPR(None,SequenceTypePR)
//  }

  test("Templated struct, one arg") {
    compile(rulePR,"Moo<Int>") shouldEqual
      TemplexPR(CallPRT(NameOrRunePRT("Moo"),List(NameOrRunePRT("Int"))))
    compile(rulePR,"Moo<*Int>") shouldEqual
      TemplexPR(CallPRT(NameOrRunePRT("Moo"),List(SharePRT(NameOrRunePRT("Int")))))
  }

  test("RWKILC") {
    compile(rulePR,"List<Int>") shouldEqual
        TemplexPR(CallPRT(NameOrRunePRT("List"),List(NameOrRunePRT("Int"))))
    compile(rulePR,"K Int") shouldEqual
        TypedPR(Some("K"),IntTypePR)
    compile(rulePR,"K<Int>") shouldEqual
        TemplexPR(CallPRT(NameOrRunePRT("K"),List(NameOrRunePRT("Int"))))
  }

  test("Templated struct, rune arg") {
    // Make sure every pattern on the way down to kind can match Int
    compile(rulePR,"Moo<R>") shouldEqual
        TemplexPR(CallPRT(NameOrRunePRT("Moo"),List(NameOrRunePRT("R"))))
  }
  test("Templated struct, multiple args") {
    // Make sure every pattern on the way down to kind can match Int
    compile(rulePR,"Moo<Int, Str>") shouldEqual
        TemplexPR(CallPRT(NameOrRunePRT("Moo"),List(NameOrRunePRT("Int"), NameOrRunePRT("Str"))))
  }
  test("Templated struct, arg is another templated struct with one arg") {
    // Make sure every pattern on the way down to kind can match Int
    compile(rulePR,"Moo<Blarg<Int>>") shouldEqual
        TemplexPR(
          CallPRT(
            NameOrRunePRT("Moo"),
            List(
                CallPRT(
                  NameOrRunePRT("Blarg"),
                  List(NameOrRunePRT("Int"))))))
  }
  test("Templated struct, arg is another templated struct with multiple arg") {
    // Make sure every pattern on the way down to kind can match Int
    compile(rulePR,"Moo<Blarg<Int, Str>>") shouldEqual
        TemplexPR(
          CallPRT(
            NameOrRunePRT("Moo"),
            List(
                CallPRT(
                  NameOrRunePRT("Blarg"),
                  List(NameOrRunePRT("Int"), NameOrRunePRT("Str"))))))
  }

  test("Repeater sequence") {
    compile(repeaterSeqRulePR, "[_ * _]") shouldEqual
      RepeaterSequencePRT(MutabilityPRT(MutableP), AnonymousRunePRT(),AnonymousRunePRT())
    compile(repeaterSeqRulePR, "[<imm> _ * _]") shouldEqual
      RepeaterSequencePRT(MutabilityPRT(ImmutableP), AnonymousRunePRT(),AnonymousRunePRT())
    compile(repeaterSeqRulePR, "[3 * Int]") shouldEqual
      RepeaterSequencePRT(MutabilityPRT(MutableP), IntPRT(3),NameOrRunePRT("Int"))
    compile(repeaterSeqRulePR, "[N * Int]") shouldEqual
        RepeaterSequencePRT(MutabilityPRT(MutableP), NameOrRunePRT("N"),NameOrRunePRT("Int"))
    compile(repeaterSeqRulePR, "[_ * Int]") shouldEqual
        RepeaterSequencePRT(MutabilityPRT(MutableP), AnonymousRunePRT(),NameOrRunePRT("Int"))
    compile(repeaterSeqRulePR, "[N * T]") shouldEqual
        RepeaterSequencePRT(MutabilityPRT(MutableP), NameOrRunePRT("N"),NameOrRunePRT("T"))
  }

  test("Regular sequence") {
    compile(manualSeqRulePR, "[]") shouldEqual
        ManualSequencePRT(List())
    compile(manualSeqRulePR, "[Int]") shouldEqual
        ManualSequencePRT(List(NameOrRunePRT("Int")))
    compile(manualSeqRulePR, "[Int, Bool]") shouldEqual
        ManualSequencePRT(List(NameOrRunePRT("Int"), NameOrRunePRT("Bool")))
    compile(manualSeqRulePR, "[_, Bool]") shouldEqual
        ManualSequencePRT(List(AnonymousRunePRT(), NameOrRunePRT("Bool")))
    compile(manualSeqRulePR, "[_, _]") shouldEqual
        ManualSequencePRT(List(AnonymousRunePRT(), AnonymousRunePRT()))
  }

//  test("Callable kind rule") {
//    compile(callableRulePR, "fn(Int)Void") shouldEqual
//        FunctionPRT(None,PackPRT(List(NameOrRunePRT("Int"))),NameOrRunePRT("Void"))
//    compile(callableRulePR, "fn(T)R") shouldEqual
//        FunctionPRT(None,PackPRT(List(NameOrRunePRT("T"))),NameOrRunePRT("R"))
//  }

  test("Prototype kind rule") {
    compile(prototypeRulePR, "fn moo(Int)Void") shouldEqual
        PrototypePRT("moo", List(NameOrRunePRT("Int")),NameOrRunePRT("Void"))
    compile(prototypeRulePR, "fn moo(T)R") shouldEqual
        PrototypePRT("moo", List(NameOrRunePRT("T")),NameOrRunePRT("R"))
  }
}
