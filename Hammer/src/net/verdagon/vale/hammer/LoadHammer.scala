package net.verdagon.vale.hammer

import net.verdagon.vale.hammer.ExpressionHammer.translate
import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.metal.{BorrowH, ShareH, Variability => _, Varying => _, _}
import net.verdagon.vale.{metal => m}
import net.verdagon.vale.templar.{types => t}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.{AddressibleLocalVariable2, ReferenceLocalVariable2}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{vassert, vfail}

object LoadHammer {

  def translateLoad(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      load2: SoftLoad2):
  (RegisterAccessH[ReferendH], List[Expression2]) = {
    val SoftLoad2(sourceExpr2, targetOwnership) = load2

    val (loadedAccessH, sourceDeferreds) =
      sourceExpr2 match {
        case LocalLookup2(ReferenceLocalVariable2(varId, variability, reference), varType2) => {
          vassert(reference == varType2)
          translateMundaneLocalLoad(hinputs, hamuts, locals, stackHeight, nodesByLine, varId, reference, targetOwnership)
        }
        case LocalLookup2(AddressibleLocalVariable2(varId, variability, localReference2), varType2) => {
          vassert(localReference2 == varType2)
          translateAddressibleLocalLoad(hinputs, hamuts, locals, stackHeight, nodesByLine, varId, variability, localReference2, targetOwnership)
        }
        case ReferenceMemberLookup2(structExpr2, memberName, memberType2) => {
          translateMundaneMemberLoad(hinputs, hamuts, locals, stackHeight, nodesByLine, structExpr2, memberType2, memberName, targetOwnership)
        }
        case AddressMemberLookup2(structExpr2, memberName, memberType2) => {
          translateAddressibleMemberLoad(hinputs, hamuts, locals, stackHeight, nodesByLine, structExpr2, memberName, memberType2, targetOwnership)
        }
        case UnknownSizeArrayLookup2(arrayExpr2, arrayType, indexExpr2) => {
          translateMundaneUnknownSizeArrayLoad(hinputs, hamuts, locals, stackHeight, nodesByLine, arrayExpr2, indexExpr2, targetOwnership)
        }
        case ArraySequenceLookup2(arrayExpr2, arrayType, indexExpr2) => {
          translateMundaneKnownSizeArrayLoad(hinputs, hamuts, locals, stackHeight, nodesByLine, arrayExpr2, indexExpr2, targetOwnership)
        }
      }

    // Note how we return the deferreds upward, see Hammer doc for why.

    (loadedAccessH, sourceDeferreds)
  }

  private def translateMundaneUnknownSizeArrayLoad(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      arrayExpr2: ReferenceExpression2,
      indexExpr2: ReferenceExpression2,
      targetOwnershipT: t.Ownership
  ): (RegisterAccessH[ReferendH], List[Expression2]) = {
    val targetOwnership = Conversions.evaluateOwnership(targetOwnershipT)

    val (Some(arrayResultLine), arrayDeferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, arrayExpr2);
    val arrayAccess = arrayResultLine.expectUnknownSizeArrayAccess()

    val (Some(indexExprResultLine), indexDeferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, indexExpr2);
    val indexAccess = indexExprResultLine.expectIntAccess()

    vassert(targetOwnership == BorrowH || targetOwnership == ShareH)

    val borrowedElementType =
      ReferenceH(targetOwnership, arrayAccess.expectedType.kind.rawArray.elementType.kind)

    // We're storing into a regular reference element of an array.
    val loadedNodeH =
      nodesByLine.addNode(
        UnknownSizeArrayLoadH(
          nodesByLine.nextId(),
          arrayAccess,
          indexAccess,
          borrowedElementType,
          targetOwnership))
    val loadedAccess =
      RegisterAccessH(loadedNodeH.registerId, borrowedElementType)

    (loadedAccess, arrayDeferreds ++ indexDeferreds)
  }

  private def translateMundaneKnownSizeArrayLoad(
    hinputs: Hinputs,
    hamuts: HamutsBox,
    locals: LocalsBox,
    stackHeight: StackHeightBox,
    nodesByLine: NodesBox,
    arrayExpr2: ReferenceExpression2,
    indexExpr2: ReferenceExpression2,
    targetOwnershipT: t.Ownership
  ): (RegisterAccessH[ReferendH], List[Expression2]) = {
    val targetOwnership = Conversions.evaluateOwnership(targetOwnershipT)

    val (Some(arrayResultLine), arrayDeferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, arrayExpr2);
    val arrayAccess = arrayResultLine.expectKnownSizeArrayAccess()

    val (Some(indexExprResultLine), indexDeferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, indexExpr2);
    val indexAccess = indexExprResultLine.expectIntAccess()

    vassert(targetOwnership == m.BorrowH || targetOwnership == m.ShareH)

    val borrowedElementType =
      ReferenceH(targetOwnership, arrayAccess.expectedType.kind.rawArray.elementType.kind)

    // We're storing into a regular reference element of an array.
    val loadedNodeH =
      nodesByLine.addNode(
        KnownSizeArrayLoadH(
          nodesByLine.nextId(),
          arrayAccess,
          indexAccess,
          borrowedElementType,
          targetOwnership))
    val loadedAccess =
      RegisterAccessH(loadedNodeH.registerId, borrowedElementType)

    (loadedAccess, arrayDeferreds ++ indexDeferreds)
  }

  private def translateAddressibleMemberLoad(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      structExpr2: ReferenceExpression2,
      memberName: FullName2[IVarName2],
      expectedType2: Coord,
      targetOwnershipT: t.Ownership
  ): (RegisterAccessH[ReferendH], List[Expression2]) = {
    val targetOwnership = Conversions.evaluateOwnership(targetOwnershipT)

    val (Some(structResultLine), structDeferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, structExpr2);

    val structRef2 =
      structExpr2.resultRegister.reference.referend match {
        case sr @ StructRef2(_) => sr
        case TupleT2(_, sr) => sr
        case PackT2(_, sr) => sr
      }
    val structDef2 = hinputs.program2.lookupStruct(structRef2)
    val memberIndex = structDef2.members.indexWhere(member => structDef2.fullName.addStep(member.name) == memberName)
    vassert(memberIndex >= 0)
    val member2 = structDef2.members(memberIndex)

    val variability = member2.variability

    val boxedType2 = member2.tyype.expectAddressMember().reference

    val (boxedTypeH) =
      TypeHammer.translateReference(hinputs, hamuts, boxedType2);

    val (boxStructRefH) =
      StructHammer.makeBox(hinputs, hamuts, variability, boxedType2, boxedTypeH)

    // We expect a borrow because structs never own boxes, they only borrow them
    val expectedStructBoxMemberType = ReferenceH(m.BorrowH, boxStructRefH)
    val expectedBorrowBoxResultType = ReferenceH(m.BorrowH, boxStructRefH)

    // We're storing into a struct's member that is a box. The stack is also
    // pointing at this box. First, get the box, then mutate what's inside.
    var varFullNameH = NameHammer.translateFullName(hinputs, hamuts, memberName)
    val loadBoxNode =
      nodesByLine.addNode(
        MemberLoadH(
          nodesByLine.nextId(),
          structResultLine.expectStructAccess(),
          memberIndex,
          m.BorrowH,
          expectedStructBoxMemberType,
          expectedBorrowBoxResultType,
          varFullNameH))
    val loadBoxAccess =
      RegisterAccessH(loadBoxNode.registerId, expectedBorrowBoxResultType)
    val loadedNodeH =
      nodesByLine.addNode(
        MemberLoadH(
          nodesByLine.nextId(),
          loadBoxAccess,
          StructHammer.BOX_MEMBER_INDEX,
          targetOwnership,
          boxedTypeH,
          ReferenceH(targetOwnership, boxedTypeH.kind),
          varFullNameH.addStep(StructHammer.BOX_MEMBER_NAME)))

    val loadedAccess =
      RegisterAccessH(loadedNodeH.registerId, boxedTypeH)
    (loadedAccess, structDeferreds)
  }

  private def translateMundaneMemberLoad(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      structExpr2: ReferenceExpression2,
      expectedMemberType2: Coord,
      memberName: FullName2[IVarName2],
      targetOwnershipT: t.Ownership
  ): (RegisterAccessH[ReferendH], List[Expression2]) = {
    val targetOwnership = Conversions.evaluateOwnership(targetOwnershipT)

    val (Some(structResultLine), structDeferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, structExpr2);

    val (expectedMemberTypeH) =
      TypeHammer.translateReference(hinputs, hamuts, expectedMemberType2);

    val structRef2 =
      structExpr2.resultRegister.reference.referend match {
        case sr @ StructRef2(_) => sr
        case TupleT2(_, sr) => sr
        case PackT2(_, sr) => sr
      }
    val structDef2 = hinputs.program2.lookupStruct(structRef2)
    val memberIndex = structDef2.members.indexWhere(member => structDef2.fullName.addStep(member.name) == memberName)
    vassert(memberIndex >= 0)

    val resultTypeH =
      ReferenceH(targetOwnership, expectedMemberTypeH.kind)

    // We're loading from a regular reference member of a struct.
    val loadedNode =
      nodesByLine.addNode(
        MemberLoadH(
          nodesByLine.nextId(),
          structResultLine.expectStructAccess(),
          memberIndex,
          targetOwnership,
          expectedMemberTypeH,
          resultTypeH,
          NameHammer.translateFullName(hinputs, hamuts, memberName)))
    val loadedAccess =
      RegisterAccessH(loadedNode.registerId, resultTypeH)
    (loadedAccess, structDeferreds)
  }

  def translateAddressibleLocalLoad(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      varId: FullName2[IVarName2],
      variability: Variability,
      localReference2: Coord,
      targetOwnershipT: t.Ownership
  ): (RegisterAccessH[ReferendH], List[Expression2]) = {
    val targetOwnership = Conversions.evaluateOwnership(targetOwnershipT)

    val local = locals.get(varId).get

    val (localTypeH) =
      TypeHammer.translateReference(hinputs, hamuts, localReference2);
    val (boxStructRefH) =
      StructHammer.makeBox(hinputs, hamuts, variability, localReference2, localTypeH)
    vassert(local.typeH.kind == boxStructRefH)

    val expectedStructBoxMemberType = ReferenceH(m.OwnH, boxStructRefH)
    val expectedBorrowBoxResultType = ReferenceH(m.BorrowH, boxStructRefH)

    // This means we're trying to load from a local variable that holds a box.
    // We need to load the box, then mutate its contents.
    val varNameH = NameHammer.translateFullName(hinputs, hamuts, varId)
    val loadBoxNode =
      nodesByLine.addNode(
        LocalLoadH(
          nodesByLine.nextId(),
          local,
          m.BorrowH,
          expectedStructBoxMemberType,
          expectedBorrowBoxResultType,
          varNameH))
    val loadBoxAccess =
      RegisterAccessH(loadBoxNode.registerId, expectedBorrowBoxResultType)

    val resultTypeH = ReferenceH(targetOwnership, localTypeH.kind)
    val loadedNode =
      nodesByLine.addNode(
        MemberLoadH(
          nodesByLine.nextId(),
          loadBoxAccess,
          StructHammer.BOX_MEMBER_INDEX,
          targetOwnership,
          localTypeH,
          resultTypeH,
          varNameH.addStep(StructHammer.BOX_MEMBER_NAME)))
    val loadedAccess =
      RegisterAccessH(loadedNode.registerId, resultTypeH)
    (loadedAccess, List())
  }

  def translateMundaneLocalLoad(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      varId: FullName2[IVarName2],
      expectedType2: Coord,
      targetOwnershipT: t.Ownership
  ): (RegisterAccessH[ReferendH], List[Expression2]) = {
    val targetOwnership = Conversions.evaluateOwnership(targetOwnershipT)


    val local = locals.get(varId) match {
      case Some(x) => x
      case None => {
        vfail("wot")
      }
    }

    val (expectedTypeH) =
      TypeHammer.translateReference(hinputs, hamuts, expectedType2);
    vassert(expectedTypeH == local.typeH)

    val resultTypeH = ReferenceH(targetOwnership, expectedTypeH.kind)

    val loadedNode =
      nodesByLine.addNode(
        LocalLoadH(
          nodesByLine.nextId(),
          local,
          targetOwnership,
          local.typeH,
          resultTypeH,
          NameHammer.translateFullName(hinputs, hamuts, varId)))
    val loadedAccess =
      RegisterAccessH(loadedNode.registerId, resultTypeH)
    (loadedAccess, List())
  }

  def translateLocalAddress(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      lookup2: LocalLookup2):
  (RegisterAccessH[ReferendH]) = {
    val LocalLookup2(localVar, type2) = lookup2;
    vassert(type2 == localVar.reference)

    val local = locals.get(localVar.id).get
    val (boxStructRefH) =
      StructHammer.makeBox(hinputs, hamuts, localVar.variability, localVar.reference, local.typeH)

    val expectedStructBoxMemberType = ReferenceH(m.OwnH, boxStructRefH)
    val expectedBorrowBoxResultType = ReferenceH(m.BorrowH, boxStructRefH)

    // This means we're trying to load from a local variable that holds a box.
    // We need to load the box, then mutate its contents.
    val loadBoxNode =
    nodesByLine.addNode(
      LocalLoadH(
        nodesByLine.nextId(),
        local,
        m.BorrowH,
        expectedStructBoxMemberType,
        expectedBorrowBoxResultType,
        NameHammer.translateFullName(hinputs, hamuts, localVar.id)))
    val loadBoxAccess =
      RegisterAccessH(loadBoxNode.registerId, expectedBorrowBoxResultType)
    (loadBoxAccess)
  }

  def translateMemberAddress(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      lookup2: AddressMemberLookup2):
  (RegisterAccessH[ReferendH], List[Expression2]) = {
    val AddressMemberLookup2(structExpr2, memberName, resultType2) = lookup2;

    val (Some(structResultLine), structDeferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, structExpr2);

    val structRef2 =
      structExpr2.resultRegister.reference.referend match {
        case sr @ StructRef2(_) => sr
        case TupleT2(_, sr) => sr
        case PackT2(_, sr) => sr
      }
    val structDef2 = hinputs.program2.lookupStruct(structRef2)
    val memberIndex = structDef2.members.indexWhere(member => structDef2.fullName.addStep(member.name) == memberName)
    vassert(memberIndex >= 0)
    val member2 = structDef2.members(memberIndex)

    val variability = member2.variability
    vassert(variability == Varying) // curious

    val boxedType2 = member2.tyype.expectAddressMember().reference

    val (boxedTypeH) =
      TypeHammer.translateReference(hinputs, hamuts, boxedType2);

    val (boxStructRefH) =
      StructHammer.makeBox(hinputs, hamuts, variability, boxedType2, boxedTypeH)

    // We expect a borrow because structs never own boxes, they only borrow them
    val expectedStructBoxMemberType = ReferenceH(m.BorrowH, boxStructRefH)
    val expectedBorrowBoxResultType = ReferenceH(m.BorrowH, boxStructRefH)

    // We're storing into a struct's member that is a box. The stack is also
    // pointing at this box. First, get the box, then mutate what's inside.
    val loadBoxNode =
    nodesByLine.addNode(
      MemberLoadH(
        nodesByLine.nextId(),
        structResultLine.expectStructAccess(),
        memberIndex,
        m.BorrowH,
        expectedStructBoxMemberType,
        expectedBorrowBoxResultType,
        NameHammer.translateFullName(hinputs, hamuts, memberName)))
    val loadBoxAccess =
      RegisterAccessH(loadBoxNode.registerId, expectedBorrowBoxResultType)

    (loadBoxAccess, structDeferreds)
  }
}
