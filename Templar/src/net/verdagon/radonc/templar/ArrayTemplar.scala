package net.verdagon.radonc.templar

import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.parser.MutableP
import net.verdagon.radonc.templar.citizen.{StructTemplar, StructTemplarCore}
import net.verdagon.radonc.templar.env.IEnvironment
import net.verdagon.radonc.templar.function.DestructorTemplar
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._

object ArrayTemplar {

  def makeArraySequenceType(env: IEnvironment, temputs0: Temputs, mutability: Mutability, size: Int, type2: Coord):
  (Temputs, ArraySequenceT2) = {
//    val tupleMutability =
//      StructTemplarCore.getCompoundTypeMutability(temputs0, List(type2))
    val tupleMutability = Templar.getMutability(temputs0, type2.referend)
    val rawArrayT2 = RawArrayT2(type2, tupleMutability)

    temputs0.arraySequenceTypes.get(size, rawArrayT2) match {
      case Some(arraySequenceT2) => (temputs0, arraySequenceT2)
      case None => {
        val arraySeqType = ArraySequenceT2(size, rawArrayT2)
        val arraySequenceRefType2 =
          Coord(
            if (tupleMutability == Mutable) Own else Share,
            arraySeqType)
        val (temputs1, _) =
          DestructorTemplar.getArrayDestructor(
            env,
            temputs0,
            arraySequenceRefType2)
        (temputs1, arraySeqType)
      }
    }
  }

  def makeUnknownSizeArrayType(env: IEnvironment, temputs0: Temputs, type2: Coord, arrayMutability: Mutability):
  (Temputs, UnknownSizeArrayT2) = {
    val rawArrayT2 = RawArrayT2(type2, arrayMutability)

    temputs0.unknownSizeArrayTypes.get(rawArrayT2) match {
      case Some(arraySequenceT2) => (temputs0, arraySequenceT2)
      case None => {
        val runtimeArrayType = UnknownSizeArrayT2(rawArrayT2)
        val runtimeArrayRefType2 =
          Coord(
            if (arrayMutability == Mutable) Own else Share,
            runtimeArrayType)
        val (temputs1, _) =
          DestructorTemplar.getArrayDestructor(
            env, temputs0, runtimeArrayRefType2)
        (temputs1, runtimeArrayType)
      }
    }
  }
}
