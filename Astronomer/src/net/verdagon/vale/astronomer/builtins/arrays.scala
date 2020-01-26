package net.verdagon.vale.astronomer.builtins

import net.verdagon.vale.astronomer._
import net.verdagon.vale.parser.{CaptureP, FinalP, MutabilityP, MutableP}
import net.verdagon.vale.scout.{IEnvironment => _, FunctionEnvironment => _, Environment => _, _}
import net.verdagon.vale.scout.patterns.AtomSP

object Arrays {
  def makeArrayFunction(mutability: MutabilityP): FunctionA = {
    val fileName = if (mutability == MutableP) { "MutArray.builtin.vale" } else { "ImmArray.builtin.vale" }
    val location = CodeLocationS(0, 0)
    val name = AbsoluteNameA(fileName, List(), FunctionNameA("!=", location))
//    List(
      FunctionA(
        name,
        false,
        TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), FunctionTemplataType),
        List(name.addStep(CodeRuneA("ArrayMutability")), name.addStep(CodeRuneA("T")), name.addStep(CodeRuneA("Generator"))),
        Map(
          name.addStep(CodeRuneA("ArrayMutability")) -> MutabilityTemplataType,
          name.addStep(CodeRuneA("T")) -> CoordTemplataType,
          name.addStep(CodeRuneA("Generator")) -> CoordTemplataType),
        List(
          ParameterA(AtomAP(CaptureA(name.addStep(CodeVarNameA("size")), FinalP), None, name.addStep(CodeRuneA("I")), None)),
          ParameterA(AtomAP(CaptureA(name.addStep(CodeVarNameA("generator")), FinalP), None, name.addStep(CodeRuneA("Generator")), None))),
        Some(name.addStep(CodeRuneA("R"))),
        List(
          EqualsAR(TemplexAR(RuneAT(name.addStep(CodeRuneA("ArrayMutability")), MutabilityTemplataType)), TemplexAR(MutabilityAT(mutability))),
          EqualsAR(TemplexAR(RuneAT(name.addStep(CodeRuneA("I")), CoordTemplataType)), TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("Int")), CoordTemplataType))),
          TemplexAR(RuneAT(name.addStep(CodeRuneA("T")), CoordTemplataType)),
          EqualsAR(
            TemplexAR(RuneAT(name.addStep(CodeRuneA("Generator")), CoordTemplataType)),
            ComponentsAR(
              CoordTemplataType,
              List(
                TemplexAR(RuneAT(name.addStep(ImplicitRuneA(0)), OwnershipTemplataType)),
                TemplexAR(
                  CallAT(
                    NameAT(ImpreciseNameA(List(), GlobalFunctionFamilyNameA("IFunction1")), TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType, CoordTemplataType), KindTemplataType)),
                    List(
                      RuneAT(name.addStep(CodeRuneA("M")), MutabilityTemplataType),
                      NameAT(ImpreciseNameA(List(), CodeTypeNameA("Int")), CoordTemplataType),
                      RuneAT(name.addStep(CodeRuneA("T")), CoordTemplataType)),
                    KindTemplataType))))),
          EqualsAR(
            TemplexAR(RuneAT(name.addStep(CodeRuneA("R")), CoordTemplataType)),
            TemplexAR(
              CallAT(
                NameAT(
                  ImpreciseNameA(List(), CodeTypeNameA("Array")),
                  TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), KindTemplataType)),
                List(RuneAT(name.addStep(CodeRuneA("ArrayMutability")), MutabilityTemplataType), RuneAT(name.addStep(CodeRuneA("T")), CoordTemplataType)),
                CoordTemplataType)))),
        CodeBodyA(
          BodyAE(
            Set(),
            BlockAE(
              Set(
                LocalVariableA(name.addStep(CodeVarNameA("size")), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed),
                LocalVariableA(name.addStep(CodeVarNameA("generator")), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
              List(
                ConstructArrayAE(
                  RuneAT(name.addStep(CodeRuneA("T")), CoordTemplataType),
                  LocalLoadAE(name.addStep(CodeVarNameA("size")), false),
                  LocalLoadAE(name.addStep(CodeVarNameA("generator")), false),
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
