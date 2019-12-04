package net.verdagon.radonc.templar

import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.templar.citizen.{StructTemplar, StructTemplarCore}
import net.verdagon.radonc.templar.env.{FunctionEnvironment, IEnvironment, NamespaceEnvironment}
import net.verdagon.radonc.templar.function.{DestructorTemplar, FunctionTemplar}
import net.verdagon.radonc.vassert

object SequenceTemplar {
  def evaluate(
    env: FunctionEnvironment,
    temputs0: Temputs,
    exprs2: List[ReferenceExpression2]):
  (Temputs, Expression2) = {

    val types2 = exprs2.map(_.resultRegister.expectReference().reference)
    if (types2.toSet.size == 1) {
      val memberType = types2.toSet.head
      // Theyre all the same type, so make it an array.
      val mutability = StructTemplarCore.getCompoundTypeMutability(temputs0, List(memberType))
      val (temputs1, arraySequenceType) = ArrayTemplar.makeArraySequenceType(env, temputs0, mutability, types2.size, memberType)
      val ownership = if (arraySequenceType.array.mutability == Mutable) Own else Share
      val finalExpr = ArraySequenceE2(exprs2, Coord(ownership, arraySequenceType), arraySequenceType)
      (temputs1, finalExpr)
    } else {
      val (temputs1, tupleType2, mutability) = makeTupleType(env.globalEnv, temputs0, types2)
      val ownership = if (mutability == Mutable) Own else Share
      val finalExpr = TupleE2(exprs2, Coord(ownership, tupleType2), tupleType2)
      (temputs1, finalExpr)
    }
  }

  private def makeTupleType(
    env: NamespaceEnvironment,
    temputs0: Temputs,
    types2: List[Coord]):
  (Temputs, TupleT2, Mutability) = {
    val (temputs1, structRef, mutability) =
      StructTemplar.makeSeqOrPackUnderstruct(env, temputs0, types2, "__Tup")

    if (types2.isEmpty)
      vassert(temputs1.lookupStruct(structRef).mutability == Immutable)
    // Make sure it's in there
    Templar.getMutability(temputs1, structRef)

    val reference =
      Coord(
        if (mutability == Mutable) Own else Share,
        structRef)

    val (temputs2, _) =
      DestructorTemplar.getCitizenDestructor(env, temputs1, reference)

    (temputs2, TupleT2(types2, structRef), mutability)
  }
}
