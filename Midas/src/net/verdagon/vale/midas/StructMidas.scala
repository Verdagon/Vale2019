package net.verdagon.vale.midas

import net.verdagon.vale.hammer._
import net.verdagon.vale.scout.{ImmutableP, MutabilityP, MutableP}
import net.verdagon.vale.templar.Own

//case class Miduts(
//    programH: ProgramH,
//    translatedReferences: Map[ReferenceH, Reference4],
//    translatedStructs: Map[StructRefH, StructDefinition4]) {
//  def addStruct(structDef4: StructDefinition4): Miduts = {
//    Miduts(programH, translatedReferences, translatedStructs.updated(structDef4.origin.getRef, structDef4))
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
//      member: StructMemberH):
//  (Miduts, StructMember4) = {
//    val StructMemberH(name, typeH) = member;
//
//    val (miduts2, field4) =
//      typeH match {
//        case AddressMemberTypeH(pointerH) => {
//          val (miduts1, reference4) = TypeMidas.translateReference(miduts0, structMutability, pointerH)
//          (miduts1, VariableAddressStructFieldType4(reference4))
//        }
//        case ReferenceMemberTypeH(referenceH) => {
//          val (miduts1, reference4) = TypeMidas.translateReference(miduts0, structMutability, referenceH)
//          (miduts1, ReferenceStructFieldType4(reference4))
//        }
//      }
//    val member4 = StructMember4(name, field4)
//    (miduts2, member4)
//  }
//
//  def translateStructMembers(miduts0: Miduts, structMutability: MutabilityP, members: List[StructMemberH]):
//  (Miduts, List[StructMember4]) = {
//    members match {
//      case Nil => (miduts0, Nil)
//      case headH :: tailH => {
//        val (miduts1, head4) = translateStructMember(miduts0, structMutability, headH)
//        val (miduts2, tail4) = translateStructMembers(miduts1, structMutability, tailH)
//        (miduts2, head4 :: tail4)
//      }
//    }
//  }
//
//  def translateStruct(
//      miduts0: Miduts,
//      structDefH: StructDefinitionH):
//  (Miduts, StructDefinition4) = {
//    miduts0.translatedStructs.get(structDefH.getRef) match {
//      case Some(structDef4) => (miduts0, structDef4)
//      case None => {
//        val StructDefinitionH(structId, humanName, globalName, mutability, maybeBaseH, eTable, edges, membersH, methodRefsH) = structDefH;
//
//        val (miduts1, maybeBase4) =
//          maybeBaseH match {
//            case None => (miduts0, None)
//            case Some(baseH) => {
//              val structDefH = miduts0.programH.structs.find(_.getRef == baseH).get
//              val (miduts1X1, structDef4) = translateStruct(miduts0, structDefH)
//              (miduts1X1, Some(structDef4.getRef))
//            }
//          }
//
//        val (miduts2, members4) = translateStructMembers(miduts1, structDefH.mutability, membersH)
//        val (midutsH, methodRefs4) = FunctionMidas.translateFunctionRefs(miduts2, methodRefsH)
//
//        val memberBaseTypesH = membersH.map(_.tyype);
//        val (miduts7, size, align) =
//          memberBaseTypesH.foldLeft((midutsH, 0, 1))({
//            case ((miduts4, prevSize, prevAlign), memberBaseTypeH) => {
//              val (miduts6, thisMemberSize, thisMemberAlign) =
//                memberBaseTypeH match {
//                  case AddressMemberTypeH(address) => (miduts4, 8, 8)
//                  case ReferenceMemberTypeH(referenceH) => {
//                    val (miduts5, pointer4) = TypeMidas.translateReference(miduts4, structDefH.mutability, referenceH)
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
//            structDefH, maybeBase4, eTable4, edges4, members4, methodRefs4, size, align)
//        val miduts8 = miduts7.addStruct(structDef4)
//        (miduts8, structDef4)
//      }
//    }
//  }
//
//  def translateInterfacePointer(
//      miduts0: Miduts, contextMutability: MutabilityP, pointerH: ReferenceH):
//  (Miduts, InterfaceReference4) = {
//    val ReferenceH(ownership, interfaceRefH : InterfaceRefH) = pointerH
//    val interfaceH = miduts0.programH.interfaces.find(_.getRef == interfaceRefH).get
//
//    println("hardcoded moved")
//    val moved = true; // pointerH.moved;
//    val mutability = interfaceH.mutability
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
//    val interfaceRef4 = InterfaceId4(interfaceRefH)
//
//    val reference4 =
//      InterfaceReference4(interfaceRef4, ownership, mutability)
//    (miduts0, reference4)
//  }
//
//  def translateStructId(structIdH: StructRefH): StructId4 = {
//    StructId4(structIdH)
//  }
//
//  def translateInterfaceId(interfaceIdH: InterfaceRefH): InterfaceId4 = {
//    InterfaceId4(interfaceIdH)
//  }
//
//  def translateStructPointer(
//      miduts0: Miduts, contextMutability: MutabilityP, pointerH: ReferenceH):
//  (Miduts, StructReference4) = {
//    val ReferenceH(ownership, structRefH : StructRefH) = pointerH
//    val structH = miduts0.programH.structs.find(_.getRef == structRefH).get
//
//    val (miduts1, structDef4) = StructMidas.translateStruct(miduts0, structH)
//
//    println("hardcoded moved")
//    val moved = true; // pointerH.moved;
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