package net.verdagon.vale.astronomer

import net.verdagon.vale.scout.{CodeLocationS, CodeRuneS, ParameterS}
import net.verdagon.vale.astronomer.externs.Externs._
import net.verdagon.vale.parser.{CaptureP, FinalP}
import net.verdagon.vale.scout.patterns.AtomSP

object makeSimpleFunction {
  def apply(
    name: AbsoluteNameA[FunctionNameA],
    params: List[(String, String)],
    retType: String,
    body: IBodyA):
  FunctionA = {
    val runeByType =
      params
        .map(_._2)
        .distinct
        .zipWithIndex
        .map({ case (tyype, index) => tyype -> name.addStep(ImplicitRuneA(index)) })
        .toMap

    val paramsA =
      params.map({ case (paramName, tyype) => simpleParam(name.addStep(CodeVarNameA(paramName)), runeByType(tyype)) })

    val paramRules = runeByType.map({ case (tyype, rune) => simpleCoordRuneAR(rune, tyype) }).toList
    val allRules = simpleCoordRuneAR(name.addStep(ReturnRuneA()), retType) :: paramRules

    FunctionA(
      name,
      false,
      FunctionTemplataType,
      List(),
      runeByType.map({ case (key, _) => (name.addStep(CodeRuneA(key)), CoordTemplataType) }).toMap,
      paramsA,
      Some(name.addStep(CodeRuneA("R"))),
      allRules,
      body)
  }


  def simpleCoordRuneAR(rune: AbsoluteNameA[IRuneA], name: String): EqualsAR = {
    EqualsAR(
      TemplexAR(RuneAT(rune, CoordTemplataType)),
      TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA(name)), CoordTemplataType)))
  }
  def simpleParam(name: AbsoluteNameA[IVarNameA], rune: AbsoluteNameA[IRuneA]): ParameterA = {
    ParameterA(AtomAP(CaptureA(name, FinalP), None, rune, None))
  }
}
