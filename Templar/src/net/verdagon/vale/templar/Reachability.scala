package net.verdagon.vale.templar

import net.verdagon.vale.templar.templata.{Prototype2, Signature2}
import net.verdagon.vale.templar.types.{InterfaceRef2, StructRef2}

import scala.collection.mutable

class Reachables(
  val functions: mutable.Set[Signature2],
  val structs: mutable.Set[StructRef2],
  val interfaces: mutable.Set[InterfaceRef2]
)

object Reachability {
  def findReachables(temputs: Temputs): Reachables = {
    val exposedFunctions =
      temputs.functions.filter(_.header.fullName.last match {
        case FunctionName2("main", _, _) => true
        case _ => false
      })
    val reachables = new Reachables(mutable.Set(), mutable.Set(), mutable.Set())
    exposedFunctions.map(_.header.toSignature).foreach(findReachables(temputs, reachables, _))
    reachables
  }
  def findReachables(temputs: Temputs, reachables: Reachables, calleeSignature: Signature2): Unit = {
    reachables.functions.add(calleeSignature)
    val function = temputs.lookupFunction(calleeSignature).get
    function.all({
      case FunctionCall2(calleePrototype, _) => findReachables(temputs, reachables, calleePrototype.toSignature)
    })
  }
}
