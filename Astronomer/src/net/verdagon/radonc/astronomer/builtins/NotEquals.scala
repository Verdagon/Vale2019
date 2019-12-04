package net.verdagon.radonc.astronomer.builtins

import net.verdagon.radonc.astronomer._
import net.verdagon.radonc.parser.{CaptureP, FinalP}
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.AtomSP

object NotEquals {
  val function =
    FunctionA(
      CodeLocation("notEquals.stl.vale", 0, 0),
      "!=",
      List("!="),
      0,
      false,
      TemplateTemplataType(List(CoordTemplataType), FunctionTemplataType),
      List("T"),
      Map("T" -> CoordTemplataType),
      List(
        ParameterS(AtomSP(Some(CaptureP("left", FinalP)), None, "T", None)),
        ParameterS(AtomSP(Some(CaptureP("right", FinalP)), None, "T", None))),
      Some("B"),
      List(
        TemplexAR(RuneAT("T", CoordTemplataType)),
        EqualsAR(TemplexAR(RuneAT("B", CoordTemplataType)), TemplexAR(NameAT("Bool", CoordTemplataType)))),
      CodeBodyA(
        BodyAE(
          Set(),
          BlockAE(
            Set(
              LocalVariable1("left", FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed),
              LocalVariable1("right", FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
            List(FunctionCallAE(
              GlobalLoadAE("not"),
              PackAE(
                List(
                  FunctionCallAE(
                    GlobalLoadAE("=="),
                    PackAE(
                      List(
                        LocalLoadAE("left", false),
                        LocalLoadAE("right", false))))))))))))
}
