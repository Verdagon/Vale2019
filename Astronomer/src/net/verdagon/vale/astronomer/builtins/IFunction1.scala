package net.verdagon.vale.astronomer.builtins

import net.verdagon.vale.astronomer._
import net.verdagon.vale.parser.{BorrowP, CaptureP, FinalP, MutableP}
import net.verdagon.vale.scout.{CodeLocationS, ParameterS}
import net.verdagon.vale.scout.patterns.{AbstractSP, AtomSP}

import scala.collection.immutable.{List, Map}

object IFunction1 {
  val interface =
    InterfaceA(
      CodeLocationS("IFunction1.builtin.vale", 0, 0),
      List(),
      "IFunction1",
      MutableP,
      Some(MutableP),
      TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType, CoordTemplataType), KindTemplataType),
      List("IFunctionM", "IFunctionP1", "IFunctionR"),
      Map(
        "IFunctionM" -> MutabilityTemplataType,
        "IFunctionP1" -> CoordTemplataType,
        "IFunctionR" -> CoordTemplataType),
      List(
        TemplexAR(RuneAT("IFunctionM", MutabilityTemplataType)),
        TemplexAR(RuneAT("IFunctionP1", CoordTemplataType)),
        TemplexAR(RuneAT("IFunctionR", CoordTemplataType))),
      List(
        FunctionA(
          CodeLocationS("IFunction1.builtin.vale", 0, 1),
          "__call", List(), 0, true,
          FunctionTemplataType,
          List(),
          Map(
            "CallM" -> MutabilityTemplataType,
            "CallP1" -> CoordTemplataType,
            "CallR" -> CoordTemplataType,
            "CallThisK" -> CoordTemplataType),
          List(
            ParameterS(AtomSP(Some(CaptureP("this", FinalP)), Some(AbstractSP), "CallBorrowThis", None)),
            ParameterS(AtomSP(Some(CaptureP("p1", FinalP)), None, "CallP1", None))),
          Some("CallR"),
          List(
            EqualsAR(TemplexAR(RuneAT("CallM", MutabilityTemplataType)), TemplexAR(NameAT("IFunctionM", MutabilityTemplataType))),
            EqualsAR(TemplexAR(RuneAT("CallP1", CoordTemplataType)), TemplexAR(NameAT("IFunctionP1", CoordTemplataType))),
            EqualsAR(TemplexAR(RuneAT("CallR", CoordTemplataType)), TemplexAR(NameAT("IFunctionR", CoordTemplataType))),
            EqualsAR(
              TemplexAR(RuneAT("CallThisK", CoordTemplataType)),
              TemplexAR(
                CallAT(
                  NameAT("IFunction1", TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType, CoordTemplataType), KindTemplataType)),
                  List(
                    NameAT("IFunctionM", MutabilityTemplataType),
                    NameAT("IFunctionP1", CoordTemplataType),
                    NameAT("IFunctionR", CoordTemplataType)),
                  CoordTemplataType))),
            EqualsAR(
              TemplexAR(RuneAT("CallBorrowThis", CoordTemplataType)),
              TemplexAR(OwnershippedAT(BorrowP, RuneAT("CallThisK", CoordTemplataType))))),
          AbstractBodyA)))

}
