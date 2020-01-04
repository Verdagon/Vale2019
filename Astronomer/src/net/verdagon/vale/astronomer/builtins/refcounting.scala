package net.verdagon.vale.astronomer.builtins

import net.verdagon.vale.astronomer._
import net.verdagon.vale.parser.{BorrowP, CaptureP, FinalP, ShareP}
import net.verdagon.vale.scout.{IEnvironment => _, FunctionEnvironment => _, Environment => _, _}
import net.verdagon.vale.scout.patterns.AtomSP

object RefCounting {
  val checkvarrc =
    FunctionA(
      CodeLocationS("__checkvarrc.builtin.vale", 0, 0),
      "__checkvarrc",
      List("__checkvarrc"),
      0,
      false,
      TemplateTemplataType(List(CoordTemplataType), FunctionTemplataType),
      List("T"),
      Map("T" -> CoordTemplataType),
      List(
        ParameterS(AtomSP(Some(CaptureP("obj", FinalP)), None, "T", None)),
        ParameterS(AtomSP(Some(CaptureP("num", FinalP)), None, "I", None))),
      Some("V"),
      List(
        EqualsAR(TemplexAR(RuneAT("I", CoordTemplataType)), TemplexAR(NameAT("Int", CoordTemplataType))),
        EqualsAR(TemplexAR(RuneAT("T", CoordTemplataType)), ComponentsAR(CoordTemplataType, List(TemplexAR(OwnershipAT(BorrowP)), TemplexAR(AnonymousRuneAT(KindTemplataType))))),
        EqualsAR(TemplexAR(RuneAT("V", CoordTemplataType)), ComponentsAR(CoordTemplataType, List(TemplexAR(OwnershipAT(ShareP)), TemplexAR(PackAT(List(), KindTemplataType)))))),
      CodeBodyA(
        BodyAE(
          Set(),
          BlockAE(
            Set(
              LocalVariable1("obj", FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed),
              LocalVariable1("num", FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
            List(
              CheckRefCountAE(
                LocalLoadAE("obj", false),
                VariableRefCount,
                FunctionCallAE(
                  // We add 1 because that "obj" is also a borrow ref
                  GlobalLoadAE("+"),
                  PackAE(
                    List(
                      LocalLoadAE("num", false),
                      IntLiteralAE(1))))),
              PackAE(List()))))))
  val checkmemberrc =
    FunctionA(
      CodeLocationS("__checkmemberrc.builtin.vale", 0, 0),
      "__checkmemberrc",
      List("__checkmemberrc"),
      0,
      false,
      TemplateTemplataType(List(CoordTemplataType), FunctionTemplataType),
      List("T"),
      Map("T" -> CoordTemplataType),
      List(
        ParameterS(AtomSP(Some(CaptureP("obj", FinalP)), None, "T", None)),
        ParameterS(AtomSP(Some(CaptureP("num", FinalP)), None, "I", None))),
      Some("V"),
      List(
        EqualsAR(TemplexAR(RuneAT("I", CoordTemplataType)), TemplexAR(NameAT("Int", CoordTemplataType))),
        EqualsAR(TemplexAR(RuneAT("T", CoordTemplataType)), ComponentsAR(CoordTemplataType, List(TemplexAR(OwnershipAT(BorrowP)), TemplexAR(AnonymousRuneAT(KindTemplataType))))),
        EqualsAR(TemplexAR(RuneAT("V", CoordTemplataType)), ComponentsAR(CoordTemplataType, List(TemplexAR(OwnershipAT(ShareP)), TemplexAR(PackAT(List(), KindTemplataType)))))),
      CodeBodyA(
        BodyAE(
          Set(),
          BlockAE(
            Set(
              LocalVariable1("obj", FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed),
              LocalVariable1("num", FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
            List(
              CheckRefCountAE(LocalLoadAE("obj", false), MemberRefCount, LocalLoadAE("num", false)),
              PackAE(List()))))))
}
