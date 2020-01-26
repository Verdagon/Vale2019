package net.verdagon.vale.astronomer.builtins

import net.verdagon.vale.astronomer._
import net.verdagon.vale.parser.{CaptureP, FinalP, ShareP}
import net.verdagon.vale.scout.{IEnvironment => _, FunctionEnvironment => _, Environment => _, _}
import net.verdagon.vale.scout.patterns.AtomSP

object Printing {
  val printlnStrName = AbsoluteNameA("printlnStr.stl.vale", List(), FunctionNameA("println", CodeLocationS(0, 0)))
  val printlnStr =
    FunctionA(
      printlnStrName,
      false,
      FunctionTemplataType,
      List(),
      Map(),
      List(
        ParameterA(AtomAP(CaptureA(printlnStrName.addStep(CodeVarNameA("line")), FinalP), None, printlnStrName.addStep(CodeRuneA("S")), None))),
      Some(printlnStrName.addStep(CodeRuneA("R"))),
      List(
        EqualsAR(TemplexAR(RuneAT(printlnStrName.addStep(CodeRuneA("S")), CoordTemplataType)), TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("Str")), CoordTemplataType))),
        EqualsAR(TemplexAR(RuneAT(printlnStrName.addStep(CodeRuneA("R")), CoordTemplataType)), TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("Void")), CoordTemplataType)))),
      CodeBodyA(
        BodyAE(
          Set(),
          BlockAE(
            Set(LocalVariableA(printlnStrName.addStep(CodeVarNameA("line")), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
            List(FunctionCallAE(
              FunctionLoadAE(ImpreciseNameA(List(), GlobalFunctionFamilyNameA("print"))),
              PackAE(
                List(
                  FunctionCallAE(
                    FunctionLoadAE(ImpreciseNameA(List(), GlobalFunctionFamilyNameA("+"))),
                    PackAE(
                      List(
                        LocalLoadAE(printlnStrName.addStep(CodeVarNameA("line")), false),
                        StrLiteralAE("\n"))))))))))))

  val printlnIntName = AbsoluteNameA("printlnInt.stl.vale", List(), FunctionNameA("println", CodeLocationS(0, 0)))
  val printlnInt =
    FunctionA(
      printlnIntName,
      false,
      FunctionTemplataType,
      List(),
      Map(),
      List(
        ParameterA(AtomAP(CaptureA(printlnIntName.addStep(CodeVarNameA("line")), FinalP), None, printlnIntName.addStep(CodeRuneA("I")), None))),
      Some(printlnIntName.addStep(CodeRuneA("R"))),
      List(
        EqualsAR(TemplexAR(RuneAT(printlnIntName.addStep(CodeRuneA("I")), CoordTemplataType)), TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("Int")), CoordTemplataType))),
        EqualsAR(TemplexAR(RuneAT(printlnIntName.addStep(CodeRuneA("R")), CoordTemplataType)), TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("Void")), CoordTemplataType)))),
      CodeBodyA(
        BodyAE(
          Set(),
          BlockAE(
            Set(LocalVariableA(printlnIntName.addStep(CodeVarNameA("line")), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
            List(FunctionCallAE(
              FunctionLoadAE(ImpreciseNameA(List(), GlobalFunctionFamilyNameA("println"))),
              PackAE(
                List(
                  FunctionCallAE(
                    FunctionLoadAE(ImpreciseNameA(List(), GlobalFunctionFamilyNameA("Str"))),
                    PackAE(
                      List(
                        LocalLoadAE(printlnIntName.addStep(CodeVarNameA("line")), false))))))))))))

  val printIntName = AbsoluteNameA("printlnInt.stl.vale", List(), FunctionNameA("println", CodeLocationS(0, 0)))
  val printInt =
    FunctionA(
      printIntName,
      false,
      FunctionTemplataType,
      List(),
      Map(),
      List(
        ParameterA(AtomAP(CaptureA(printIntName.addStep(CodeVarNameA("line")), FinalP), None, printIntName.addStep(CodeRuneA("I")), None))),
      Some(printIntName.addStep(CodeRuneA("R"))),
      List(
        EqualsAR(TemplexAR(RuneAT(printIntName.addStep(CodeRuneA("I")), CoordTemplataType)), TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("Int")), CoordTemplataType))),
        EqualsAR(TemplexAR(RuneAT(printIntName.addStep(CodeRuneA("R")), CoordTemplataType)), TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("Void")), CoordTemplataType)))),
      CodeBodyA(
        BodyAE(
          Set(),
          BlockAE(
            Set(LocalVariableA(printIntName.addStep(CodeVarNameA("line")), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
            List(FunctionCallAE(
              FunctionLoadAE(ImpreciseNameA(List(), GlobalFunctionFamilyNameA("print"))),
              PackAE(
                List(
                  FunctionCallAE(
                    FunctionLoadAE(ImpreciseNameA(List(), GlobalFunctionFamilyNameA("Str"))),
                    PackAE(
                      List(
                        LocalLoadAE(printIntName.addStep(CodeVarNameA("line")), false))))))))))))
}
