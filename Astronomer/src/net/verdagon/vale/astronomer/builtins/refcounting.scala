package net.verdagon.vale.astronomer.builtins

import net.verdagon.vale.astronomer._
import net.verdagon.vale.parser.{BorrowP, CaptureP, FinalP, ShareP}
import net.verdagon.vale.scout.{IEnvironment => _, FunctionEnvironment => _, Environment => _, _}
import net.verdagon.vale.scout.patterns.AtomSP

object RefCounting {
  val checkVarRcName = AbsoluteNameA("checkVarRc.stl.vale", List(), FunctionNameA("checkvarrc", CodeLocationS(0, 0)))
  val checkvarrc =
    FunctionA(
      checkVarRcName,
      false,
      TemplateTemplataType(List(CoordTemplataType), FunctionTemplataType),
      List(checkVarRcName.addStep(CodeRuneA("T"))),
      Map(checkVarRcName.addStep(CodeRuneA("T")) -> CoordTemplataType),
      List(
        ParameterA(AtomAP(CaptureA(checkVarRcName.addStep(CodeVarNameA("obj")), FinalP), None, checkVarRcName.addStep(CodeRuneA("T")), None)),
        ParameterA(AtomAP(CaptureA(checkVarRcName.addStep(CodeVarNameA("num")), FinalP), None, checkVarRcName.addStep(CodeRuneA("I")), None))),
      Some(checkVarRcName.addStep(CodeRuneA("V"))),
      List(
        EqualsAR(TemplexAR(RuneAT(checkVarRcName.addStep(CodeRuneA("I")), CoordTemplataType)), TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("Int")), CoordTemplataType))),
        EqualsAR(TemplexAR(RuneAT(checkVarRcName.addStep(CodeRuneA("T")), CoordTemplataType)), ComponentsAR(CoordTemplataType, List(TemplexAR(OwnershipAT(BorrowP)), TemplexAR(RuneAT(checkVarRcName.addStep(ImplicitRuneA(0)), KindTemplataType))))),
        EqualsAR(TemplexAR(RuneAT(checkVarRcName.addStep(CodeRuneA("V")), CoordTemplataType)), ComponentsAR(CoordTemplataType, List(TemplexAR(OwnershipAT(ShareP)), TemplexAR(PackAT(List(), KindTemplataType)))))),
      CodeBodyA(
        BodyAE(
          Set(),
          BlockAE(
            Set(
              LocalVariableA(checkVarRcName.addStep(CodeVarNameA("obj")), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed),
              LocalVariableA(checkVarRcName.addStep(CodeVarNameA("num")), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
            List(
              CheckRefCountAE(
                LocalLoadAE(checkVarRcName.addStep(CodeVarNameA("obj")), false),
                VariableRefCount,
                FunctionCallAE(
                  // We add 1 because that "obj" is also a borrow ref
                  FunctionLoadAE(ImpreciseNameA(List(), GlobalFunctionFamilyNameA("+"))),
                  PackAE(
                    List(
                      LocalLoadAE(checkVarRcName.addStep(CodeVarNameA("num")), false),
                      IntLiteralAE(1))))),
              PackAE(List()))))))

  val checkMemberRcName = AbsoluteNameA("checkVarRc.stl.vale", List(), FunctionNameA("checkmemberrc", CodeLocationS(0, 0)))
  val checkmemberrc =
    FunctionA(
      checkMemberRcName,
      false,
      TemplateTemplataType(List(CoordTemplataType), FunctionTemplataType),
      List(checkMemberRcName.addStep(CodeRuneA("T"))),
      Map(checkMemberRcName.addStep(CodeRuneA("T")) -> CoordTemplataType),
      List(
        ParameterA(AtomAP(CaptureA(checkMemberRcName.addStep(CodeVarNameA("obj")), FinalP), None, checkMemberRcName.addStep(CodeRuneA("T")), None)),
        ParameterA(AtomAP(CaptureA(checkMemberRcName.addStep(CodeVarNameA("num")), FinalP), None, checkMemberRcName.addStep(CodeRuneA("I")), None))),
      Some(checkMemberRcName.addStep(CodeRuneA("V"))),
      List(
        EqualsAR(TemplexAR(RuneAT(checkMemberRcName.addStep(CodeRuneA("I")), CoordTemplataType)), TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("Int")), CoordTemplataType))),
        EqualsAR(TemplexAR(RuneAT(checkMemberRcName.addStep(CodeRuneA("T")), CoordTemplataType)), ComponentsAR(CoordTemplataType, List(TemplexAR(OwnershipAT(BorrowP)), TemplexAR(RuneAT(checkMemberRcName.addStep(ImplicitRuneA(0)), KindTemplataType))))),
        EqualsAR(TemplexAR(RuneAT(checkMemberRcName.addStep(CodeRuneA("V")), CoordTemplataType)), ComponentsAR(CoordTemplataType, List(TemplexAR(OwnershipAT(ShareP)), TemplexAR(PackAT(List(), KindTemplataType)))))),
      CodeBodyA(
        BodyAE(
          Set(),
          BlockAE(
            Set(
              LocalVariableA(checkMemberRcName.addStep(CodeVarNameA("obj")), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed),
              LocalVariableA(checkMemberRcName.addStep(CodeVarNameA("num")), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
            List(
              CheckRefCountAE(LocalLoadAE(checkMemberRcName.addStep(CodeVarNameA("obj")), false), MemberRefCount, LocalLoadAE(checkMemberRcName.addStep(CodeVarNameA("num")), false)),
              PackAE(List()))))))
}
