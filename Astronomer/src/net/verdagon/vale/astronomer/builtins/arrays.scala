package net.verdagon.vale.astronomer.builtins

import net.verdagon.vale.astronomer._
import net.verdagon.vale.parser.{CaptureP, FinalP, MutabilityP, MutableP}
import net.verdagon.vale.scout.{IEnvironment => _, FunctionEnvironment => _, Environment => _, _}
import net.verdagon.vale.scout.patterns.AtomSP

object Arrays {
  def makeArrayFunction(mutability: MutabilityP): FunctionA = {
    val location = if (mutability == MutableP) { CodeLocationS(0, 0) } else { CodeLocationS(1, 0) }
    val name = FunctionNameA("Array", location)
//    List(
      FunctionA(
        name,
        false,
        TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), FunctionTemplataType),
        List(CodeRuneA("ArrayMutability"), CodeRuneA("T"), CodeRuneA("Generator")),
        Map(
          CodeRuneA("ArrayMutability") -> MutabilityTemplataType,
          CodeRuneA("I") -> CoordTemplataType,
          CodeRuneA("T") -> CoordTemplataType,
          CodeRuneA("Generator") -> CoordTemplataType,
          CodeRuneA("M") -> MutabilityTemplataType,
          CodeRuneA("R") -> MutabilityTemplataType,
          ImplicitRuneA(0) -> OwnershipTemplataType),
        List(
          ParameterA(AtomAP(CaptureA(CodeVarNameA("size"), FinalP), None, CodeRuneA("I"), None)),
          ParameterA(AtomAP(CaptureA(CodeVarNameA("generator"), FinalP), None, CodeRuneA("Generator"), None))),
        Some(CodeRuneA("R")),
        List(
          EqualsAR(TemplexAR(RuneAT(CodeRuneA("ArrayMutability"), MutabilityTemplataType)), TemplexAR(MutabilityAT(mutability))),
          EqualsAR(TemplexAR(RuneAT(CodeRuneA("I"), CoordTemplataType)), TemplexAR(NameAT(CodeTypeNameA("Int"), CoordTemplataType))),
          TemplexAR(RuneAT(CodeRuneA("T"), CoordTemplataType)),
          EqualsAR(
            TemplexAR(RuneAT(CodeRuneA("Generator"), CoordTemplataType)),
            ComponentsAR(
              CoordTemplataType,
              List(
                TemplexAR(RuneAT(ImplicitRuneA(0), OwnershipTemplataType)),
                TemplexAR(
                  CallAT(
                    NameAT(CodeTypeNameA("IFunction1"), TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType, CoordTemplataType), KindTemplataType)),
                    List(
                      RuneAT(CodeRuneA("M"), MutabilityTemplataType),
                      NameAT(CodeTypeNameA("Int"), CoordTemplataType),
                      RuneAT(CodeRuneA("T"), CoordTemplataType)),
                    KindTemplataType))))),
          EqualsAR(
            TemplexAR(RuneAT(CodeRuneA("R"), CoordTemplataType)),
            TemplexAR(
              CallAT(
                NameAT(
                  CodeTypeNameA("Array"),
                  TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), KindTemplataType)),
                List(RuneAT(CodeRuneA("ArrayMutability"), MutabilityTemplataType), RuneAT(CodeRuneA("T"), CoordTemplataType)),
                CoordTemplataType)))),
        CodeBodyA(
          BodyAE(
            List(),
            BlockAE(
              List(
                LocalVariableA(CodeVarNameA("size"), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed),
                LocalVariableA(CodeVarNameA("generator"), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
              List(
                ConstructArrayAE(
                  RuneAT(CodeRuneA("T"), CoordTemplataType),
                  LocalLoadAE(CodeVarNameA("size"), false),
                  LocalLoadAE(CodeVarNameA("generator"), false),
                  mutability))))))
//      FunctionA(
//        CodeLocation("Array.builtin.vale", 0, 0),
//        "Array",
//        List("Array"),
//        0,
//        false,
//        TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), FunctionTemplataType),
//        List("ArrayMutability", "T"),
//        Map(
//          "ArrayMutability" -> MutabilityTemplataType,
//          "T" -> CoordTemplataType,
//          "S" -> CoordTemplataType),
//        List(
//          ParameterS(AtomSP(Some(CaptureP("seq", FinalP)), None, "S", None))),
//        Some("R"),
//        List(
//          EqualsAR(TemplexAR(RuneAT("ArrayMutability", MutabilityTemplataType)), TemplexAR(MutabilityAT(mutability))),
//          EqualsAR(
//            TemplexAR(RuneAT("S", CoordTemplataType)),
//            TemplexAR(RepeaterSequenceAT(RuneAT("N", IntegerTemplataType), RuneAT("T", CoordTemplataType), CoordTemplataType))),
//          EqualsAR(
//            TemplexAR(RuneAT("R", CoordTemplataType)),
//            TemplexAR(
//              CallAT(
//                NameAT("Array", TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), KindTemplataType)),
//                List(
//                  RuneAT("ArrayMutability", MutabilityTemplataType),
//                  RuneAT("T", CoordTemplataType)),
//                CoordTemplataType)))),
//        CodeBodyA(
//          BodyAE(
//            ?

  }
}
