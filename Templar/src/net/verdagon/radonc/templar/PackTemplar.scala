package net.verdagon.radonc.templar

import net.verdagon.radonc.astronomer.PackAE
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.templar.ExpressionTemplar.evaluateAndCoerceToReferenceExpression
import net.verdagon.radonc.templar.citizen.StructTemplar
import net.verdagon.radonc.templar.env.{FunctionEnvironment, IEnvironment, NamespaceEnvironment}
import net.verdagon.radonc.templar.function.DestructorTemplar
import net.verdagon.radonc.{vassert, vfail}

object PackTemplar {
  val PACK_NAME = "__Pack"

  val emptyPackType: PackT2 = PackT2(List(), Program2.emptyPackStructRef)
  val emptyPackReference: Coord = Coord(Share, emptyPackType)

  val emptyPackExpression: PackE2 = PackE2(List(), Coord(Share, PackTemplar.emptyPackType), PackTemplar.emptyPackType)

  def evaluate(temputs0: Temputs, fate0: FunctionEnvironment, packExpr1: PackAE):
  (Temputs, FunctionEnvironment, ReferenceExpression2, Set[Coord]) = {

    val (temputs2, fate1, argExprs2, returnsFromElements) =
      ExpressionTemplar.evaluateAndCoerceToReferenceExpressions(temputs0, fate0, packExpr1.elements);

    // Simplify 1-element packs
    val (temputs4, finalExpr2) =
      argExprs2 match {
        case List(onlyExpr2) => {
          (temputs2, onlyExpr2)
        }
        case _ => {
          val types2 =
            argExprs2.map(
              expr2 => expr2.resultRegister.expectReference().reference)
          val (temputs3, packType2, mutability) = makePackType(fate0.globalEnv, temputs2, types2)
          val ownership = if (mutability == Mutable) Own else Share
          val expression = PackE2(argExprs2, Coord(ownership, packType2), packType2)
          (temputs3, expression)
        }
      };

    (temputs4, fate1, finalExpr2, returnsFromElements)
  }

  def makePackType(env: NamespaceEnvironment, temputs0: Temputs, types2: List[Coord]):
  (Temputs, PackT2, Mutability) = {
    val (temputs1, structRef, mutability) =
      StructTemplar.makeSeqOrPackUnderstruct(env, temputs0, types2, PACK_NAME)

    if (types2.isEmpty)
      vassert(temputs1.lookupStruct(structRef).mutability == Immutable)

    val packType2 = PackT2(types2, structRef);

    val packReferenceType2 = Coord(if (mutability == Mutable) Own else Share, packType2)

    val (temputs2, _) =
      DestructorTemplar.getCitizenDestructor(env, temputs1, packReferenceType2)

    (temputs2, packType2, mutability)
  }
}
