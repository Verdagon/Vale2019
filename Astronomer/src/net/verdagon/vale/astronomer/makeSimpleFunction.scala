package net.verdagon.vale.astronomer

import net.verdagon.vale.scout.{CodeLocationS, CodeRuneS, ParameterS}
import net.verdagon.vale.astronomer.externs.Externs._
import net.verdagon.vale.parser.{CaptureP, FinalP}
import net.verdagon.vale.scout.patterns.AtomSP

object makeSimpleFunction {
  def apply(
    name: FunctionNameA,
    params: List[(String, String)],
    retType: String,
    body: IBodyA):
  FunctionA = {
    val runeByType =
      params
        .map(_._2)
        .distinct
        .zipWithIndex
        .map({ case (tyype, index) => tyype -> ImplicitRuneA(index) })
        .toMap

    val paramsA =
      params.map({ case (paramName, tyype) => simpleParam(CodeVarNameA(paramName), runeByType(tyype)) })

    val returnRune = ReturnRuneA()
    val paramRules = runeByType.map({ case (tyype, rune) => simpleCoordRuneAR(rune, tyype) }).toList
    val allRules = simpleCoordRuneAR(returnRune, retType) :: paramRules

    FunctionA(
      name,
      false,
      FunctionTemplataType,
      List(),
      runeByType.map({ case (_, rune) => (rune, CoordTemplataType) }).toMap,
      paramsA,
      Some(returnRune),
      allRules,
      body)
  }


  def simpleCoordRuneAR(rune: IRuneA, name: String): EqualsAR = {
    EqualsAR(
      TemplexAR(RuneAT(rune, CoordTemplataType)),
      TemplexAR(NameAT(CodeTypeNameA(name), CoordTemplataType)))
  }
  def simpleParam(name: IVarNameA, rune: IRuneA): ParameterA = {
    ParameterA(AtomAP(CaptureA(name, FinalP), None, rune, None))
  }
}
