package net.verdagon.vale.astronomer.builtins

import net.verdagon.vale.astronomer._
import net.verdagon.vale.parser.{CaptureP, FinalP}
import net.verdagon.vale.scout.{IEnvironment => _, FunctionEnvironment => _, Environment => _, _}
import net.verdagon.vale.scout.patterns.AtomSP

object NotEquals {
  val name = AbsoluteNameA("notEquals.stl.vale", List(), FunctionNameA("!=", CodeLocationS(0, 0)))
  val function =
    FunctionA(
      name,
      false,
      TemplateTemplataType(List(CoordTemplataType), FunctionTemplataType),
      List(name.addStep(CodeRuneA("T"))),
      Map(name.addStep(CodeRuneA("T")) -> CoordTemplataType),
      List(
        ParameterA(AtomAP(CaptureA(name.addStep(CodeVarNameA("left")), FinalP), None, name.addStep(CodeRuneA("T")), None)),
        ParameterA(AtomAP(CaptureA(name.addStep(CodeVarNameA("right")), FinalP), None, name.addStep(CodeRuneA("T")), None))),
      Some(name.addStep(CodeRuneA("B"))),
      List(
        TemplexAR(RuneAT(name.addStep(CodeRuneA("T")), CoordTemplataType)),
        EqualsAR(TemplexAR(RuneAT(name.addStep(CodeRuneA("B")), CoordTemplataType)), TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("Bool")), CoordTemplataType)))),
      CodeBodyA(
        BodyAE(
          Set(),
          BlockAE(
            Set(
              LocalVariableA(name.addStep(CodeVarNameA("left")), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed),
              LocalVariableA(name.addStep(CodeVarNameA("right")), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
            List(FunctionCallAE(
              FunctionLoadAE(ImpreciseNameA(List(), GlobalFunctionFamilyNameA("not"))),
              PackAE(
                List(
                  FunctionCallAE(
                    FunctionLoadAE(ImpreciseNameA(List(), GlobalFunctionFamilyNameA("=="))),
                    PackAE(
                      List(
                        LocalLoadAE(name.addStep(CodeVarNameA("left")), false),
                        LocalLoadAE(name.addStep(CodeVarNameA("right")), false))))))))))))
}
