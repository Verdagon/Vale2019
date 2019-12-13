package net.verdagon.radonc.parser.rules

import net.verdagon.radonc.parser.VParser._
import net.verdagon.radonc.parser._
import net.verdagon.radonc.vfail
import org.scalatest.{FunSuite, Matchers}

class KindRuleTests extends FunSuite with Matchers {
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

  test("Empty Kind rule") {
    compile(rulePR, ":Kind") shouldEqual TypedPR(None,KindTypePR)
  }

  test("Kind with rune") {
    compile(rulePR, "#T: Kind") shouldEqual TypedPR(Some("T"),KindTypePR)
    //runedTKind("T")
  }

  test("Kind with destructure only") {
    compile(rulePR, ":Kind[_]") shouldEqual
        ComponentsPR(TypedPR(None,KindTypePR),List(TemplexPR(AnonymousRunePRT())))
//        KindPR(None, KindTypePR, None, None)
  }

  test("Kind with rune and destructure") {
    compile(rulePR, "#T: Kind[_]") shouldEqual
        ComponentsPR(TypedPR(Some("T"),KindTypePR),List(TemplexPR(AnonymousRunePRT())))
    compile(rulePR, "#T: Kind[mut]") shouldEqual
        ComponentsPR(
          TypedPR(Some("T"),KindTypePR),
          List(TemplexPR(MutabilityPRT(MutableP))))
  }

  test("Kind matches plain Int") {
    compile(rulePR, "Int") shouldEqual
        TemplexPR(NamePRT("Int"))
  }

  test("Kind with value") {
    compile(rulePR, "#T: Kind = Int") shouldEqual
        EqualsPR(TypedPR(Some("T"),KindTypePR),TemplexPR(NamePRT("Int")))
  }

  test("Kind with destructure and value") {
    compile(rulePR, "#T: Kind[_] = Int") shouldEqual
        EqualsPR(
          ComponentsPR(TypedPR(Some("T"),KindTypePR),List(TemplexPR(AnonymousRunePRT()))),
          TemplexPR(NamePRT("Int")))
  }

  test("Kind with sequence in value spot") {
    compile(rulePR, "#T: Kind = [Int, Bool]") shouldEqual
        EqualsPR(
          TypedPR(Some("T"),KindTypePR),
          TemplexPR(
            ManualSequencePRT(
              List(NamePRT("Int"), NamePRT("Bool")))))
  }

  test("Braces without Kind is sequence") {
    compile(rulePR, "[Int, Bool]") shouldEqual
        TemplexPR(
          ManualSequencePRT(
            List(NamePRT("Int"), NamePRT("Bool"))))
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
      TemplexPR(CallPRT(NamePRT("Moo"),List(NamePRT("Int"))))
    compile(rulePR,"Moo<*Int>") shouldEqual
      TemplexPR(CallPRT(NamePRT("Moo"),List(SharePRT(NamePRT("Int")))))
  }

  test("RWKILC") {
    compile(rulePR,"List<Int>") shouldEqual
        TemplexPR(CallPRT(NamePRT("List"),List(NamePRT("Int"))))
    compile(rulePR,"#K: Int") shouldEqual
        TypedPR(Some("K"),IntTypePR)
    compile(rulePR,"#K<Int>") shouldEqual
        TemplexPR(CallPRT(RunePRT("K"),List(NamePRT("Int"))))
  }

  test("Templated struct, rune arg") {
    // Make sure every pattern on the way down to kind can match Int
    compile(rulePR,"Moo<#R>") shouldEqual
        TemplexPR(CallPRT(NamePRT("Moo"),List(RunePRT("R"))))
  }
  test("Templated struct, multiple args") {
    // Make sure every pattern on the way down to kind can match Int
    compile(rulePR,"Moo<Int, Str>") shouldEqual
        TemplexPR(CallPRT(NamePRT("Moo"),List(NamePRT("Int"), NamePRT("Str"))))
  }
  test("Templated struct, arg is another templated struct with one arg") {
    // Make sure every pattern on the way down to kind can match Int
    compile(rulePR,"Moo<Blarg<Int>>") shouldEqual
        TemplexPR(
          CallPRT(
            NamePRT("Moo"),
            List(
                CallPRT(
                  NamePRT("Blarg"),
                  List(NamePRT("Int"))))))
  }
  test("Templated struct, arg is another templated struct with multiple arg") {
    // Make sure every pattern on the way down to kind can match Int
    compile(rulePR,"Moo<Blarg<Int, Str>>") shouldEqual
        TemplexPR(
          CallPRT(
            NamePRT("Moo"),
            List(
                CallPRT(
                  NamePRT("Blarg"),
                  List(NamePRT("Int"), NamePRT("Str"))))))
  }

  test("Repeater sequence") {
    compile(repeaterSeqRulePR, "[_ * _]") shouldEqual
      RepeaterSequencePRT(MutabilityPRT(MutableP), AnonymousRunePRT(),AnonymousRunePRT())
    compile(repeaterSeqRulePR, "[:imm _ * _]") shouldEqual
      RepeaterSequencePRT(MutabilityPRT(ImmutableP), AnonymousRunePRT(),AnonymousRunePRT())
    compile(repeaterSeqRulePR, "[3 * Int]") shouldEqual
      RepeaterSequencePRT(MutabilityPRT(MutableP), IntPRT(3),NamePRT("Int"))
    compile(repeaterSeqRulePR, "[#N * Int]") shouldEqual
        RepeaterSequencePRT(MutabilityPRT(MutableP), RunePRT("N"),NamePRT("Int"))
    compile(repeaterSeqRulePR, "[_ * Int]") shouldEqual
        RepeaterSequencePRT(MutabilityPRT(MutableP), AnonymousRunePRT(),NamePRT("Int"))
    compile(repeaterSeqRulePR, "[#N * #T]") shouldEqual
        RepeaterSequencePRT(MutabilityPRT(MutableP), RunePRT("N"),RunePRT("T"))
  }

  test("Regular sequence") {
    compile(manualSeqRulePR, "[]") shouldEqual
        ManualSequencePRT(List())
    compile(manualSeqRulePR, "[Int]") shouldEqual
        ManualSequencePRT(List(NamePRT("Int")))
    compile(manualSeqRulePR, "[Int, Bool]") shouldEqual
        ManualSequencePRT(List(NamePRT("Int"), NamePRT("Bool")))
    compile(manualSeqRulePR, "[_, Bool]") shouldEqual
        ManualSequencePRT(List(AnonymousRunePRT(), NamePRT("Bool")))
    compile(manualSeqRulePR, "[_, _]") shouldEqual
        ManualSequencePRT(List(AnonymousRunePRT(), AnonymousRunePRT()))
  }

  test("Callable kind rule") {
    compile(callableRulePR, "fn(Int)Void") shouldEqual
        FunctionPRT(None,PackPRT(List(NamePRT("Int"))),NamePRT("Void"))
    compile(callableRulePR, "fn(#T)#R") shouldEqual
        FunctionPRT(None,PackPRT(List(RunePRT("T"))),RunePRT("R"))
  }

  test("Prototype kind rule") {
    compile(prototypeRulePR, "fn moo(Int)Void") shouldEqual
        PrototypePRT("moo", List(NamePRT("Int")),NamePRT("Void"))
    compile(prototypeRulePR, "fn moo(#T)#R") shouldEqual
        PrototypePRT("moo", List(RunePRT("T")),RunePRT("R"))
  }
}
