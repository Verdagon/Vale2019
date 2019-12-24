package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.vfail

object TypeHammer {
  def translateMembers(hinputs: Hinputs, hamuts0: Hamuts, members: List[StructMember2]):
  (Hamuts, List[StructMemberH]) = {
    members match {
      case Nil => (hamuts0, Nil)
      case headMember2 :: tailMembers2 => {
        val (hamuts1, headMemberH) = translateMember(hinputs, hamuts0, headMember2)
        val (hamuts2, tailMembersH) = translateMembers(hinputs, hamuts1, tailMembers2)
        (hamuts2, headMemberH :: tailMembersH)
      }
    }
  }

  def translateMember(hinputs: Hinputs, hamuts: Hamuts, member2: StructMember2):
  (Hamuts, StructMemberH) = {
    val (hamutsH, memberH) =
      member2.tyype match {
        case ReferenceMemberType2(coord) => {
          TypeHammer.translateReference(hinputs, hamuts, coord)
        }
        case AddressMemberType2(coord) => {
          val (hamuts1, referenceH) =
            TypeHammer.translateReference(hinputs, hamuts, coord)
          val (hamuts2, boxStructRefH) =
            StructHammer.makeBox(hinputs, hamuts1, member2.variability, coord, referenceH)
          // The stack owns the box, closure structs just borrow it.
          (hamuts2, ReferenceH(Borrow, boxStructRefH))
        }
      }
    (hamutsH, StructMemberH(member2.name, member2.variability, memberH))
  }

//
//  def translateType(hinputs: Hinputs, hamuts: Hamuts, tyype: BaseType2):
//  (Hamuts, BaseTypeH) = {
//    tyype match {
//      case Addressible2(innerType) => {
//        val (hamuts1, pointerH) = translatePointer(hinputs, hamuts, innerType)
//        (hamuts1, AddressibleH(pointerH))
//      }
//      case Coord(ownership, innerType) => {
//        val (hamuts1, pointerH) = translate(hinputs, hamuts, innerType)
//        (hamuts1, PointerH(ownership, pointerH))
//      }
//    }
//  }

  //  def translatePointer(tyype: Coord): PointerH = {
  //  }

  def translateFunction(
      hinputs: Hinputs, hamuts0: Hamuts, tyype: FunctionT2):
  (Hamuts, FunctionTH) = {
    val FunctionT2(paramTypes, returnType) = tyype;
    val (hamuts1, returnTypeH) = translateReference(hinputs, hamuts0, tyype.returnType)
    val (hamuts2, paramTypesH) = translateReferences(hinputs, hamuts1, tyype.paramTypes)
    (hamuts2, FunctionTH(paramTypesH, returnTypeH))
  }

  def translateKind(hinputs: Hinputs, hamuts0: Hamuts, tyype: Kind):
  (Hamuts, ReferendH) = {
    tyype match {
      case Never2() => (hamuts0, NeverH())
      case Int2() => (hamuts0, IntH())
      case Bool2() => (hamuts0, BoolH())
      case Float2() => (hamuts0, FloatH())
      case Str2() => (hamuts0, StrH())
      case Void2() => (hamuts0, VoidH())
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
  (Hamuts, ReferenceH[ReferendH]) = {
    val Coord(ownership, innerType) = coord;
    val (hamuts1, innerH) = translateKind(hinputs, hamuts0, innerType);
    (hamuts1, ReferenceH(ownership, innerH))
  }

  def translateReferences(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      references2: List[Coord]):
  (Hamuts, List[ReferenceH[ReferendH]]) = {
    references2 match {
      case Nil => (hamuts0, Nil)
      case headReference2 :: tailPointers2 => {
        val (hamuts1, headPointerH) = translateReference(hinputs, hamuts0, headReference2)
        val (hamuts2, tailPointersH) = translateReferences(hinputs, hamuts1, tailPointers2)
        (hamuts2, headPointerH :: tailPointersH)
      }
    }
  }

//  def checkReference(baseTypeH: BaseTypeH): ReferenceH = {
//    baseTypeH match {
//      case AddressibleH(_) => vfail("Expected a pointer, was an addressible!")
//      case p @ ReferenceH(_, _) => p
//    }
//  }

  def checkConversion(expected: ReferenceH[ReferendH], actual: ReferenceH[ReferendH]): Unit = {
    if (actual != expected) {
      vfail("Expected a " + expected + " but was a " + actual);
    }
  }

  def translateKnownSizeArray(hinputs: Hinputs, hamuts0: Hamuts, type2: ArraySequenceT2) = {
    val (hamuts1, memberReferenceH) = TypeHammer.translateReference(hinputs, hamuts0, type2.array.memberType)
    (hamuts1, KnownSizeArrayTH(type2.size, RawArrayTH(memberReferenceH)))
  }

  def translateUnknownSizeArray(hinputs: Hinputs, hamuts0: Hamuts, type2: UnknownSizeArrayT2) = {
    val (hamuts1, memberReferenceH) = TypeHammer.translateReference(hinputs, hamuts0, type2.array.memberType)
    (hamuts1, UnknownSizeArrayTH(RawArrayTH(memberReferenceH)))
  }
}
