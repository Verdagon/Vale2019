package net.verdagon.radonc.hammer

import net.verdagon.radonc.hinputs.Hinputs
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.vfail

object TypeHammer {
  def translateMembers(hinputs: Hinputs, hamuts0: Hamuts, members: List[StructMember2]):
  (Hamuts, List[StructMember3]) = {
    members match {
      case Nil => (hamuts0, Nil)
      case headMember2 :: tailMembers2 => {
        val (hamuts1, headMember3) = translateMember(hinputs, hamuts0, headMember2)
        val (hamuts2, tailMembers3) = translateMembers(hinputs, hamuts1, tailMembers2)
        (hamuts2, headMember3 :: tailMembers3)
      }
    }
  }

  def translateMember(hinputs: Hinputs, hamuts: Hamuts, member2: StructMember2):
  (Hamuts, StructMember3) = {
    val (hamuts3, member3) =
      member2.tyype match {
        case ReferenceMemberType2(coord) => {
          TypeHammer.translateReference(hinputs, hamuts, coord)
        }
        case AddressMemberType2(coord) => {
          val (hamuts1, reference3) =
            TypeHammer.translateReference(hinputs, hamuts, coord)
          val (hamuts2, boxStructRef3) =
            StructHammer.makeBox(hinputs, hamuts1, member2.variability, coord, reference3)
          // The stack owns the box, closure structs just borrow it.
          (hamuts2, Reference3(Borrow, boxStructRef3))
        }
      }
    (hamuts3, StructMember3(member2.name, member2.variability, member3))
  }

//
//  def translateType(hinputs: Hinputs, hamuts: Hamuts, tyype: BaseType2):
//  (Hamuts, BaseType3) = {
//    tyype match {
//      case Addressible2(innerType) => {
//        val (hamuts1, pointer3) = translatePointer(hinputs, hamuts, innerType)
//        (hamuts1, Addressible3(pointer3))
//      }
//      case Coord(ownership, innerType) => {
//        val (hamuts1, pointer3) = translate(hinputs, hamuts, innerType)
//        (hamuts1, Pointer3(ownership, pointer3))
//      }
//    }
//  }

  //  def translatePointer(tyype: Coord): Pointer3 = {
  //  }

  def translateFunction(
      hinputs: Hinputs, hamuts0: Hamuts, tyype: FunctionT2):
  (Hamuts, FunctionT3) = {
    val FunctionT2(paramTypes, returnType) = tyype;
    val (hamuts1, returnType3) = translateReference(hinputs, hamuts0, tyype.returnType)
    val (hamuts2, paramTypes3) = translateReferences(hinputs, hamuts1, tyype.paramTypes)
    (hamuts2, FunctionT3(paramTypes3, returnType3))
  }

  def translateKind(hinputs: Hinputs, hamuts0: Hamuts, tyype: Kind):
  (Hamuts, Referend3) = {
    tyype match {
      case Never2() => (hamuts0, Never3())
      case Int2() => (hamuts0, Int3())
      case Bool2() => (hamuts0, Bool3())
      case Float2() => (hamuts0, Float3())
      case Str2() => (hamuts0, Str3())
      case Void2() => (hamuts0, Void3())
      case t : FunctionT2 => translateFunction(hinputs, hamuts0, t)
      case s@ StructRef2(_) => StructHammer.translateStructRef(hinputs, hamuts0, s)

      case i @ InterfaceRef2(_) => StructHammer.translateInterfaceRef(hinputs, hamuts0, i)

//      // A Closure2 is really just a struct ref under the hood. The dinstinction is only meaningful
//      // to the Templar.
//      case OrdinaryClosure2(_, handleStructRef, prototype) => translate(hinputs, hamuts0, handleStructRef)
//      case TemplatedClosure2(_, handleStructRef, terry) => translate(hinputs, hamuts0, handleStructRef)
      case OverloadSet(_, _, understructRef2) => {
        StructHammer.translateStructRef(hinputs, hamuts0, understructRef2)
      }

      // A PackT2 is really just a struct ref under the hood. The dinstinction is only meaningful
      // to the Templar.
      case p @ PackT2(_, underlyingStruct) => StructHammer.translateStructRef(hinputs, hamuts0, underlyingStruct)
      case p @ TupleT2(_, underlyingStruct) => StructHammer.translateStructRef(hinputs, hamuts0, underlyingStruct)
      case a @ ArraySequenceT2(_, _) => translateKnownSizeArray(hinputs, hamuts0, a)
      case a @ UnknownSizeArrayT2(_) => translateUnknownSizeArray(hinputs, hamuts0, a)
    }
  }

  def translateReference(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      coord: Coord):
  (Hamuts, Reference3[Referend3]) = {
    val Coord(ownership, innerType) = coord;
    val (hamuts1, inner3) = translateKind(hinputs, hamuts0, innerType);
    (hamuts1, Reference3(ownership, inner3))
  }

  def translateReferences(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      references2: List[Coord]):
  (Hamuts, List[Reference3[Referend3]]) = {
    references2 match {
      case Nil => (hamuts0, Nil)
      case headReference2 :: tailPointers2 => {
        val (hamuts1, headPointer3) = translateReference(hinputs, hamuts0, headReference2)
        val (hamuts2, tailPointers3) = translateReferences(hinputs, hamuts1, tailPointers2)
        (hamuts2, headPointer3 :: tailPointers3)
      }
    }
  }

//  def checkReference(baseType3: BaseType3): Reference3 = {
//    baseType3 match {
//      case Addressible3(_) => vfail("Expected a pointer, was an addressible!")
//      case p @ Reference3(_, _) => p
//    }
//  }

  def checkConversion(expected: Reference3[Referend3], actual: Reference3[Referend3]): Unit = {
    if (actual != expected) {
      vfail("Expected a " + expected + " but was a " + actual);
    }
  }

  def translateKnownSizeArray(hinputs: Hinputs, hamuts0: Hamuts, type2: ArraySequenceT2) = {
    val (hamuts1, memberReference3) = TypeHammer.translateReference(hinputs, hamuts0, type2.array.memberType)
    (hamuts1, KnownSizeArrayT3(type2.size, RawArrayT3(memberReference3)))
  }

  def translateUnknownSizeArray(hinputs: Hinputs, hamuts0: Hamuts, type2: UnknownSizeArrayT2) = {
    val (hamuts1, memberReference3) = TypeHammer.translateReference(hinputs, hamuts0, type2.array.memberType)
    (hamuts1, UnknownSizeArrayT3(RawArrayT3(memberReference3)))
  }
}
