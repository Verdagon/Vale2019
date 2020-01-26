package net.verdagon.vale.astronomer.builtins

import net.verdagon.vale.astronomer._
import net.verdagon.vale.parser.{BorrowP, CaptureP, FinalP, MutableP}
import net.verdagon.vale.scout.{CodeLocationS, ParameterS}
import net.verdagon.vale.scout.patterns.{AbstractSP, AtomSP}

import scala.collection.immutable.{List, Map}

object IFunction1 {
  val ifunction1Name = AbsoluteNameA("IFunction1.stl.vale", List(), FunctionNameA("IFunction1", CodeLocationS(0, 0)))
  val callName = ifunction1Name.addStep(FunctionNameA("__call", CodeLocationS(1, 0)))
  val interface =
    InterfaceA(
      ifunction1Name,
      MutableP,
      Some(MutableP),
      TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType, CoordTemplataType), KindTemplataType),
      List(ifunction1Name.addStep(CodeRuneA("M")), ifunction1Name.addStep(CodeRuneA("P1")), ifunction1Name.addStep(CodeRuneA("R"))),
      Map(
        ifunction1Name.addStep(CodeRuneA("M")) -> MutabilityTemplataType,
        ifunction1Name.addStep(CodeRuneA("P1")) -> CoordTemplataType,
        ifunction1Name.addStep(CodeRuneA("R")) -> CoordTemplataType),
      List(
        TemplexAR(RuneAT(ifunction1Name.addStep(CodeRuneA("M")), MutabilityTemplataType)),
        TemplexAR(RuneAT(ifunction1Name.addStep(CodeRuneA("P1")), CoordTemplataType)),
        TemplexAR(RuneAT(ifunction1Name.addStep(CodeRuneA("R")), CoordTemplataType))),
      List(
        FunctionA(
          callName,
          true,
          FunctionTemplataType,
          List(),
          Map(callName.addStep(CodeRuneA("ThisK")) -> CoordTemplataType),
          List(
            ParameterA(AtomAP(CaptureA(callName.addStep(CodeVarNameA("this")), FinalP), Some(AbstractAP), callName.addStep(CodeRuneA("BorrowThis")), None)),
            ParameterA(AtomAP(CaptureA(callName.addStep(CodeVarNameA("p1")), FinalP), None, ifunction1Name.addStep(CodeRuneA("P1")), None))),
          Some(ifunction1Name.addStep(CodeRuneA("R"))),
          List(
            EqualsAR(
              TemplexAR(RuneAT(callName.addStep(CodeRuneA("ThisK")), CoordTemplataType)),
              TemplexAR(
                CallAT(
                  NameAT(ImpreciseNameA(List(), CodeTypeNameA("IFunction1")), TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType, CoordTemplataType), KindTemplataType)),
                  List(
                    RuneAT(ifunction1Name.addStep(CodeRuneA("M")), MutabilityTemplataType),
                    RuneAT(ifunction1Name.addStep(CodeRuneA("P1")), CoordTemplataType),
                    RuneAT(ifunction1Name.addStep(CodeRuneA("R")), CoordTemplataType)),
                  CoordTemplataType))),
            EqualsAR(
              TemplexAR(RuneAT(callName.addStep(CodeRuneA("BorrowThis")), CoordTemplataType)),
              TemplexAR(OwnershippedAT(BorrowP, RuneAT(callName.addStep(CodeRuneA("ThisK")), CoordTemplataType))))),
          AbstractBodyA)))

}
