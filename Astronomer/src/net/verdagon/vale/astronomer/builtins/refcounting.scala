package net.verdagon.vale.astronomer.builtins

import net.verdagon.vale.astronomer._
import net.verdagon.vale.parser.{BorrowP, CaptureP, FinalP, ShareP}
import net.verdagon.vale.scout.{IEnvironment => _, FunctionEnvironment => _, Environment => _, _}
import net.verdagon.vale.scout.patterns.AtomSP

object RefCounting {
  val checkVarRcName = FunctionNameA("checkvarrc", CodeLocationS(0, 0))
  val checkvarrc =
    FunctionA(
      checkVarRcName,
      false,
      TemplateTemplataType(List(CoordTemplataType), FunctionTemplataType),
      List(CodeRuneA("T")),
      Map(CodeRuneA("T") -> CoordTemplataType),
      List(
        ParameterA(AtomAP(CaptureA(CodeVarNameA("obj"), FinalP), None, CodeRuneA("T"), None)),
        ParameterA(AtomAP(CaptureA(CodeVarNameA("num"), FinalP), None, CodeRuneA("I"), None))),
      Some(CodeRuneA("V")),
      List(
        EqualsAR(TemplexAR(RuneAT(CodeRuneA("I"), CoordTemplataType)), TemplexAR(NameAT(CodeTypeNameA("Int"), CoordTemplataType))),
        EqualsAR(TemplexAR(RuneAT(CodeRuneA("T"), CoordTemplataType)), ComponentsAR(CoordTemplataType, List(TemplexAR(OwnershipAT(BorrowP)), TemplexAR(RuneAT(ImplicitRuneA(0), KindTemplataType))))),
        EqualsAR(TemplexAR(RuneAT(CodeRuneA("V"), CoordTemplataType)), ComponentsAR(CoordTemplataType, List(TemplexAR(OwnershipAT(ShareP)), TemplexAR(PackAT(List(), KindTemplataType)))))),
      CodeBodyA(
        BodyAE(
          List(),
          BlockAE(
            List(
              LocalVariableA(CodeVarNameA("obj"), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed),
              LocalVariableA(CodeVarNameA("num"), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
            List(
              CheckRefCountAE(
                LocalLoadAE(CodeVarNameA("obj"), false),
                VariableRefCount,
                FunctionCallAE(
                  // We add 1 because that "obj" is also a borrow ref
                  FunctionLoadAE(GlobalFunctionFamilyNameA("+")),
                  PackAE(
                    List(
                      LocalLoadAE(CodeVarNameA("num"), false),
                      IntLiteralAE(1))))),
              PackAE(List()))))))

  val checkMemberRcName = FunctionNameA("checkmemberrc", CodeLocationS(0, 0))
  val checkmemberrc =
    FunctionA(
      checkMemberRcName,
      false,
      TemplateTemplataType(List(CoordTemplataType), FunctionTemplataType),
      List(CodeRuneA("T")),
      Map(CodeRuneA("T") -> CoordTemplataType),
      List(
        ParameterA(AtomAP(CaptureA(CodeVarNameA("obj"), FinalP), None, CodeRuneA("T"), None)),
        ParameterA(AtomAP(CaptureA(CodeVarNameA("num"), FinalP), None, CodeRuneA("I"), None))),
      Some(CodeRuneA("V")),
      List(
        EqualsAR(TemplexAR(RuneAT(CodeRuneA("I"), CoordTemplataType)), TemplexAR(NameAT(CodeTypeNameA("Int"), CoordTemplataType))),
        EqualsAR(TemplexAR(RuneAT(CodeRuneA("T"), CoordTemplataType)), ComponentsAR(CoordTemplataType, List(TemplexAR(OwnershipAT(BorrowP)), TemplexAR(RuneAT(ImplicitRuneA(0), KindTemplataType))))),
        EqualsAR(TemplexAR(RuneAT(CodeRuneA("V"), CoordTemplataType)), ComponentsAR(CoordTemplataType, List(TemplexAR(OwnershipAT(ShareP)), TemplexAR(PackAT(List(), KindTemplataType)))))),
      CodeBodyA(
        BodyAE(
          List(),
          BlockAE(
            List(
              LocalVariableA(CodeVarNameA("obj"), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed),
              LocalVariableA(CodeVarNameA("num"), FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
            List(
              CheckRefCountAE(LocalLoadAE(CodeVarNameA("obj"), false), MemberRefCount, LocalLoadAE(CodeVarNameA("num"), false)),
              PackAE(List()))))))
}
