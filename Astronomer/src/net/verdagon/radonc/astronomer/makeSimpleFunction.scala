package net.verdagon.radonc.astronomer

import net.verdagon.radonc.astronomer.externs.Externs.{simpleCoordRuneAR, simpleParam}
import net.verdagon.radonc.scout.CodeLocation

import scala.collection.immutable.List

object makeSimpleFunction {
  def apply(
    functionName: String,
    params: List[(String, String)],
    retType: String,
    body: IBodyA):
  FunctionA = {
    val runeByType =
      params
        .map(_._2)
        .distinct
        .zipWithIndex
        .map({ case (tyype, index) => tyype -> index.toString })
        .toMap

    val paramsA =
      params.map({ case (name, tyype) => simpleParam(name, runeByType(tyype)) })

    val paramRules = runeByType.map({ case (tyype, rune) => simpleCoordRuneAR(rune, tyype) }).toList
    val allRules = simpleCoordRuneAR("R", retType) :: paramRules

    FunctionA(
      CodeLocation(functionName + ".builtin.vale", 0, 0),
      functionName,
      List(),
      0,
      false,
      FunctionTemplataType,
      List(),
      runeByType.values.map(_ -> CoordTemplataType).toMap,
      paramsA,
      Some("R"),
      allRules,
      body)
  }
}
