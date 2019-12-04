package net.verdagon.radonc.astronomer.builtins

import net.verdagon.radonc.astronomer._
import net.verdagon.radonc.astronomer.externs.Externs.makeExtern
import net.verdagon.radonc.parser.FinalP
import net.verdagon.radonc.scout.{LocalVariable1, NotUsed, Used}

import scala.collection.immutable.List

object Forwarders {
  val forwarders =
    List[FunctionA](
      makeForwarder("print", List(("output", "Str")), "Void", "__print"),
      makeForwarder("getch", List(), "Int", "__getch"),
      makeForwarder("+", List(("a", "Int"), ("b", "Int")), "Int", "__addIntInt"),
      makeForwarder("+", List(("a", "Float"), ("b", "Float")), "Float", "__addFloatFloat"),
      makeForwarder("+", List(("a", "Str"), ("b", "Str")), "Str", "__addStrStr"),
      makeForwarder("*", List(("left", "Int"), ("right", "Int")), "Int", "__multiplyIntInt"),
      makeForwarder("*", List(("left", "Float"), ("right", "Float")), "Float", "__multiplyFloatFloat"),
      makeForwarder("mod", List(("left", "Int"), ("right", "Int")), "Int", "__mod"),
      makeForwarder("==", List(("left", "Bool"), ("right", "Bool")), "Bool", "__eqBoolBool"),
      makeForwarder("==", List(("left", "Int"), ("right", "Int")), "Bool", "__eqIntInt"),
      makeForwarder("==", List(("left", "Str"), ("right", "Str")), "Bool", "__eqStrStr"),
      makeForwarder("Float", List(("left", "Int")), "Float", "__castIntFloat"),
      makeForwarder("Int", List(("left", "Float")), "Int", "__castFloatInt"),
      makeForwarder("Str", List(("left", "Int")), "Str", "__castIntStr"),
      makeForwarder("Str", List(("left", "Float")), "Str", "__castFloatStr"),
      makeForwarder("and", List(("left", "Bool"), ("right", "Bool")), "Bool", "__and"),
      makeForwarder("not", List(("output", "Bool")), "Bool", "__not"),
      makeForwarder("-", List(("left", "Int")), "Int", "__negateInt"),
      makeForwarder("sqrt", List(("x", "Float")), "Float", "__sqrt"),
      makeForwarder("-", List(("left", "Float")), "Float", "__negateFloat"),
      makeForwarder("-", List(("left", "Int"), ("right", "Int")), "Int", "__subtractIntInt"),
      makeForwarder("-", List(("left", "Float"), ("right", "Float")), "Float", "__subtractFloatFloat"),
      makeForwarder("<", List(("left", "Float"), ("right", "Float")), "Bool", "__lessThanFloat"),
      makeForwarder(">", List(("left", "Float"), ("right", "Float")), "Bool", "__greaterThanFloat"),
      makeForwarder("<", List(("left", "Int"), ("right", "Int")), "Bool", "__lessThanInt"),
      makeForwarder(">", List(("left", "Int"), ("right", "Int")), "Bool", "__greaterThanInt"),
      makeForwarder("<=", List(("left", "Int"), ("right", "Int")), "Bool", "__lessThanOrEqInt"),
      makeForwarder(">=", List(("left", "Int"), ("right", "Int")), "Bool", "__greaterThanOrEqInt"))

  def makeForwarder(name: String, params: List[(String, String)], ret: String, callee: String): FunctionA = {
    makeSimpleFunction(
      name,
      params,
      ret,
      CodeBodyA(
        BodyAE(
          Set(),
          BlockAE(
            params.map(_._1).toSet.map(param => {
              LocalVariable1(param, FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)
            }),
            List(
              FunctionCallAE(
                GlobalLoadAE(callee),
                PackAE(
                    params.map(param => LocalLoadAE(param._1, false)))))))))
  }
}
