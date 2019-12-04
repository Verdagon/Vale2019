package net.verdagon.radonc.astronomer.builtins

import net.verdagon.radonc.astronomer._
import net.verdagon.radonc.parser.{CaptureP, FinalP, MutabilityP}
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.AtomSP

object Arrays {
  def makeArrayFunction(mutability: MutabilityP): FunctionA = {
//    List(
      FunctionA(
        CodeLocation("__Array.builtin.vale", 0, 0),
        "__Array",
        List("__Array"),
        0,
        false,
        TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), FunctionTemplataType),
        List("ArrayMutability", "T", "F"),
        Map(
          "ArrayMutability" -> MutabilityTemplataType,
          "T" -> CoordTemplataType,
          "F" -> CoordTemplataType),
        List(
          ParameterS(AtomSP(Some(CaptureP("size", FinalP)), None, "I", None)),
          ParameterS(AtomSP(Some(CaptureP("generator", FinalP)), None, "F", None))),
        Some("R"),
        List(
          EqualsAR(TemplexAR(RuneAT("ArrayMutability", MutabilityTemplataType)), TemplexAR(MutabilityAT(mutability))),
          EqualsAR(TemplexAR(RuneAT("I", CoordTemplataType)), TemplexAR(NameAT("Int", CoordTemplataType))),
          TemplexAR(RuneAT("T", CoordTemplataType)),
          EqualsAR(
            TemplexAR(RuneAT("F", CoordTemplataType)),
            ComponentsAR(
              CoordTemplataType,
              List(TemplexAR(AnonymousRuneAT(OwnershipTemplataType)), TemplexAR(RuneAT("FR", KindTemplataType))))),
          EqualsAR(
            TemplexAR(RuneAT("R", CoordTemplataType)),
            TemplexAR(
              CallAT(
                NameAT(
                  "__Array",
                  TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), KindTemplataType)),
                List(RuneAT("ArrayMutability", MutabilityTemplataType), RuneAT("T", CoordTemplataType)),
                CoordTemplataType)))),
        CodeBodyA(
          BodyAE(
            Set(),
            BlockAE(
              Set(
                LocalVariable1("size", FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed),
                LocalVariable1("generator", FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
              List(
                ConstructArrayAE(
                  NameAT("T", CoordTemplataType),
                  LocalLoadAE("size", false),
                  LocalLoadAE("generator", false),
                  mutability))))))
//      FunctionA(
//        CodeLocation("__Array.builtin.vale", 0, 0),
//        "__Array",
//        List("__Array"),
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
//                NameAT("__Array", TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), KindTemplataType)),
//                List(
//                  RuneAT("ArrayMutability", MutabilityTemplataType),
//                  RuneAT("T", CoordTemplataType)),
//                CoordTemplataType)))),
//        CodeBodyA(
//          BodyAE(
//            ?

  }
}
