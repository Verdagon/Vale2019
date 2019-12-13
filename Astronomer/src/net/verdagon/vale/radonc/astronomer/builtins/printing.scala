package net.verdagon.vale.astronomer.builtins

import net.verdagon.vale.astronomer._
import net.verdagon.vale.parser.{CaptureP, FinalP, ShareP}
import net.verdagon.vale.scout._
import net.verdagon.vale.scout.patterns.AtomSP

object Printing {
  val printlnStr =
    FunctionA(
      CodeLocation("printlnStr.stl.vale", 0, 0),
      "println",
      List("println(:Str)"),
      0,
      false,
      FunctionTemplataType,
      List(),
      Map(),
      List(
        ParameterS(AtomSP(Some(CaptureP("line", FinalP)), None, "S", None))),
      Some("R"),
      List(
        EqualsAR(TemplexAR(RuneAT("S", CoordTemplataType)), TemplexAR(NameAT("Str", CoordTemplataType))),
        EqualsAR(TemplexAR(RuneAT("R", CoordTemplataType)), TemplexAR(NameAT("Void", CoordTemplataType)))),
      CodeBodyA(
        BodyAE(
          Set(),
          BlockAE(
            Set(LocalVariable1("line", FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
            List(FunctionCallAE(
              GlobalLoadAE("print"),
              PackAE(
                List(
                  FunctionCallAE(
                    GlobalLoadAE("+"),
                    PackAE(
                      List(
                        LocalLoadAE("line", false),
                        StrLiteralAE("\n"))))))))))))

  val printlnInt =
    FunctionA(
      CodeLocation("printlnInt.stl.vale", 0, 0),
      "println",
      List("println(:Int)"),
      0, false,
      FunctionTemplataType,
      List(),
      Map(),
      List(
        ParameterS(AtomSP(Some(CaptureP("line", FinalP)), None, "I", None))),
      Some("R"),
      List(
        EqualsAR(TemplexAR(RuneAT("I", CoordTemplataType)), TemplexAR(NameAT("Int", CoordTemplataType))),
        EqualsAR(TemplexAR(RuneAT("R", CoordTemplataType)), TemplexAR(NameAT("Void", CoordTemplataType)))),
      CodeBodyA(
        BodyAE(
          Set(),
          BlockAE(
            Set(LocalVariable1("line", FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
            List(FunctionCallAE(
              GlobalLoadAE("println"),
              PackAE(
                List(
                  FunctionCallAE(
                    GlobalLoadAE("Str"),
                    PackAE(
                      List(
                        LocalLoadAE("line", false))))))))))))
  val printInt =
    FunctionA(
      CodeLocation("printInt.stl.vale", 0, 0),
      "print",
      List("print(:Int)"),
      0,
      false,
      FunctionTemplataType,
      List(),
      Map(),
      List(
        ParameterS(AtomSP(Some(CaptureP("line", FinalP)), None, "I", None))),
      Some("R"),
      List(
        EqualsAR(TemplexAR(RuneAT("I", CoordTemplataType)), TemplexAR(NameAT("Int", CoordTemplataType))),
        EqualsAR(TemplexAR(RuneAT("R", CoordTemplataType)), TemplexAR(NameAT("Void", CoordTemplataType)))),
      CodeBodyA(
        BodyAE(
          Set(),
          BlockAE(
            Set(LocalVariable1("line", FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
            List(FunctionCallAE(
              GlobalLoadAE("print"),
              PackAE(
                List(
                  FunctionCallAE(
                    GlobalLoadAE("Str"),
                    PackAE(
                      List(
                        LocalLoadAE("line", false))))))))))))
}
