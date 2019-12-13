package net.verdagon.vale.midas

import net.verdagon.vale.hammer._
import net.verdagon.vale.scout.{ImmutableP, MutabilityP, MutableP}
import net.verdagon.vale.templar.Own

//case class Miduts(
//    program3: Program3,
//    translatedReferences: Map[Reference3, Reference4],
//    translatedStructs: Map[StructRef3, StructDefinition4]) {
//  def addStruct(structDef4: StructDefinition4): Miduts = {
//    Miduts(program3, translatedReferences, translatedStructs.updated(structDef4.origin.getRef, structDef4))
//  }
//}
//
//object StructMidas {
//  def estimateSize(tyype: Concrete4):
//  Int = {
//    tyype match {
//      case Int4() => 8 // Though, we only use 6 of these bytes.
//      case Bool4() => 1
//      case Float4() => 4
//      case _ : FunctionT4 => 8
//      case Struct4(structDef4) => structDef4.size
//    }
//  }
//
//  def translateStructMember(
//      miduts0: Miduts,
//      structMutability: MutabilityP,
//      member: StructMember3):
//  (Miduts, StructMember4) = {
//    val StructMember3(name, type3) = member;
//
//    val (miduts2, field4) =
//      type3 match {
//        case AddressMemberType3(pointer3) => {
//          val (miduts1, reference4) = TypeMidas.translateReference(miduts0, structMutability, pointer3)
//          (miduts1, VariableAddressStructFieldType4(reference4))
//        }
//        case ReferenceMemberType3(reference3) => {
//          val (miduts1, reference4) = TypeMidas.translateReference(miduts0, structMutability, reference3)
//          (miduts1, ReferenceStructFieldType4(reference4))
//        }
//      }
//    val member4 = StructMember4(name, field4)
//    (miduts2, member4)
//  }
//
//  def translateStructMembers(miduts0: Miduts, structMutability: MutabilityP, members: List[StructMember3]):
//  (Miduts, List[StructMember4]) = {
//    members match {
//      case Nil => (miduts0, Nil)
//      case head3 :: tail3 => {
//        val (miduts1, head4) = translateStructMember(miduts0, structMutability, head3)
//        val (miduts2, tail4) = translateStructMembers(miduts1, structMutability, tail3)
//        (miduts2, head4 :: tail4)
//      }
//    }
//  }
//
//  def translateStruct(
//      miduts0: Miduts,
//      structDef3: StructDefinition3):
//  (Miduts, StructDefinition4) = {
//    miduts0.translatedStructs.get(structDef3.getRef) match {
//      case Some(structDef4) => (miduts0, structDef4)
//      case None => {
//        val StructDefinition3(structId, humanName, globalName, mutability, maybeBase3, eTable, edges, members3, methodRefs3) = structDef3;
//
//        val (miduts1, maybeBase4) =
//          maybeBase3 match {
//            case None => (miduts0, None)
//            case Some(base3) => {
//              val structDef3 = miduts0.program3.structs.find(_.getRef == base3).get
//              val (miduts1X1, structDef4) = translateStruct(miduts0, structDef3)
//              (miduts1X1, Some(structDef4.getRef))
//            }
//          }
//
//        val (miduts2, members4) = translateStructMembers(miduts1, structDef3.mutability, members3)
//        val (miduts3, methodRefs4) = FunctionMidas.translateFunctionRefs(miduts2, methodRefs3)
//
//        val memberBaseTypes3 = members3.map(_.tyype);
//        val (miduts7, size, align) =
//          memberBaseTypes3.foldLeft((miduts3, 0, 1))({
//            case ((miduts4, prevSize, prevAlign), memberBaseType3) => {
//              val (miduts6, thisMemberSize, thisMemberAlign) =
//                memberBaseType3 match {
//                  case AddressMemberType3(address) => (miduts4, 8, 8)
//                  case ReferenceMemberType3(reference3) => {
//                    val (miduts5, pointer4) = TypeMidas.translateReference(miduts4, structDef3.mutability, reference3)
//                    (miduts5, pointer4.size, pointer4.align)
//                  }
//                }
//              // Put this member at the next multiple of align.
//              val thisMemberBegin =
//                if (prevSize % thisMemberAlign == 0) {
//                  prevSize
//                } else {
//                  prevSize / thisMemberAlign * thisMemberAlign + thisMemberAlign
//                }
//              val newSize = thisMemberBegin + thisMemberSize
//              val newAlign = Math.max(prevAlign, thisMemberAlign)
//              (miduts6, newSize, newAlign)
//            }
//          })
//
//        val eTable4 = vfail("not yet")
//        val edges4 = vfail("not yet")
//
//        val structDef4 =
//          StructDefinition4(
//            structDef3, maybeBase4, eTable4, edges4, members4, methodRefs4, size, align)
//        val miduts8 = miduts7.addStruct(structDef4)
//        (miduts8, structDef4)
//      }
//    }
//  }
//
//  def translateInterfacePointer(
//      miduts0: Miduts, contextMutability: MutabilityP, pointer3: Reference3):
//  (Miduts, InterfaceReference4) = {
//    val Reference3(ownership, interfaceRef3 : InterfaceRef3) = pointer3
//    val interface3 = miduts0.program3.interfaces.find(_.getRef == interfaceRef3).get
//
//    println("hardcoded moved")
//    val moved = true; // pointer3.moved;
//    val mutability = interface3.mutability
//    // And ownership
//    // And pointerFromImmutable
//
//    if (contextMutability == ImmutableP && ownership == Own) {
//      vassert(false); // ImmutableP objects cannot own mutables
//    }
//    if (contextMutability == ImmutableP && moved) {
//      vassert(false); // Can't move out from an immutable object
//    }
//    if (ownership == Own && mutability == ImmutableP) {
//      vassert(false); // Can't own an immutable
//    }
//
//    val interfaceRef4 = InterfaceId4(interfaceRef3)
//
//    val reference4 =
//      InterfaceReference4(interfaceRef4, ownership, mutability)
//    (miduts0, reference4)
//  }
//
//  def translateStructId(structId3: StructRef3): StructId4 = {
//    StructId4(structId3)
//  }
//
//  def translateInterfaceId(interfaceId3: InterfaceRef3): InterfaceId4 = {
//    InterfaceId4(interfaceId3)
//  }
//
//  def translateStructPointer(
//      miduts0: Miduts, contextMutability: MutabilityP, pointer3: Reference3):
//  (Miduts, StructReference4) = {
//    val Reference3(ownership, structRef3 : StructRef3) = pointer3
//    val struct3 = miduts0.program3.structs.find(_.getRef == structRef3).get
//
//    val (miduts1, structDef4) = StructMidas.translateStruct(miduts0, struct3)
//
//    println("hardcoded moved")
//    val moved = true; // pointer3.moved;
//    val mutability = structDef4.mutability
//    val small = structDef4.size <= 32
//    // and ownership
//    // And pointerFromImmutable
//
//    if (contextMutability == ImmutableP && ownership == Own) {
//      vassert(false); // ImmutableP objects cannot own mutables
//    }
//    if (contextMutability == ImmutableP && moved) {
//      vassert(false); // Can't move out from an immutable object
//    }
//    if (ownership == Own && mutability == ImmutableP) {
//      vassert(false); // Can't own an immutable
//    }
//
//    val isInline =
//      if (mutability == ImmutableP && small) {
//        true
//      } else if (mutability == MutableP && ownership == Own && !moved) {
//        true
//      } else {
//        false
//      };
//
//    val reference4 =
//      if (isInline) {
//        InlineStructReference4(structDef4.getRef, ownership, mutability, structDef4.size, structDef4.align)
//      } else {
//        FarStructReference4(structDef4.getRef, ownership, mutability)
//      };
//    (miduts1, reference4)
//  }
//}