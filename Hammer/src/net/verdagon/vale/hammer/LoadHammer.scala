package net.verdagon.vale.hammer

import net.verdagon.vale.hammer.ExpressionHammer.{addNode, newId, translate}
import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.{AddressibleLocalVariable2, ReferenceLocalVariable2, VariableId2}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{vassert, vfail}

object LoadHammer {

  def translateLoad(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      load2: SoftLoad2):
  (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH], List[Expression2]) = {
    val SoftLoad2(sourceExpr2, targetOwnership) = load2

    val (hamuts5, locals5, stackHeight5, nodesByLine5, loadedAccessH, sourceDeferreds) =
      sourceExpr2 match {
        case LocalLookup2(ReferenceLocalVariable2(varId, variability, reference), varType2) => {
          vassert(reference == varType2)
          translateMundaneLocalLoad(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, varId, reference, targetOwnership)
        }
        case LocalLookup2(AddressibleLocalVariable2(varId, variability, localReference2), varType2) => {
          vassert(localReference2 == varType2)
          translateAddressibleLocalLoad(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, varId, variability, localReference2, targetOwnership)
        }
        case ReferenceMemberLookup2(structExpr2, memberName, memberType2) => {
          translateMundaneMemberLoad(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, structExpr2, memberType2, memberName, targetOwnership)
        }
        case AddressMemberLookup2(structExpr2, memberName, varName, memberType2) => {
          translateAddressibleMemberLoad(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, structExpr2, memberName, memberType2, targetOwnership)
        }
        case UnknownSizeArrayLookup2(arrayExpr2, arrayType, indexExpr2) => {
          translateMundaneUnknownSizeArrayLoad(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, arrayExpr2, indexExpr2, targetOwnership)
        }
        case ArraySequenceLookup2(arrayExpr2, arrayType, indexExpr2) => {
          translateMundaneKnownSizeArrayLoad(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, arrayExpr2, indexExpr2, targetOwnership)
        }
      }

    // Note how we return the deferreds upward, see Hammer doc for why.

    (hamuts5, locals5, stackHeight5, nodesByLine5, loadedAccessH, sourceDeferreds)
  }

  private def translateMundaneUnknownSizeArrayLoad(
      hinputs: Hinputs,
      hamuts2: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      arrayExpr2: ReferenceExpression2,
      indexExpr2: ReferenceExpression2,
      targetOwnership: Ownership
  ): (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH], List[Expression2]) = {
    val (hamutsH, locals1, stackHeight1, nodesByLine1, Some(arrayResultLine), arrayDeferreds) =
      translate(hinputs, hamuts2, locals0, stackHeight0, nodesByLine0, arrayExpr2);
    val arrayAccess = arrayResultLine.expectUnknownSizeArrayAccess()

    val (hamuts4, locals2, stackHeight2, nodesByLine2, Some(indexExprResultLine), indexDeferreds) =
      translate(hinputs, hamutsH, locals1, stackHeight1, nodesByLine1, indexExpr2);
    val indexAccess = indexExprResultLine.expectIntAccess()

    vassert(targetOwnership == Borrow || targetOwnership == Share)

    val borrowedElementType =
      ReferenceH(targetOwnership, arrayAccess.expectedType.kind.rawArray.elementType.kind)

    // We're storing into a regular reference element of an array.
    val (nodesByLineH, loadedNodeH) =
      addNode(
        nodesByLine2,
        UnknownSizeArrayLoadH(
          newId(nodesByLine2),
          arrayAccess,
          indexAccess,
          borrowedElementType,
          targetOwnership))
    val loadedAccess =
      RegisterAccessH(loadedNodeH.registerId, borrowedElementType)

    (hamuts4, locals2, stackHeight2, nodesByLineH, loadedAccess, arrayDeferreds ++ indexDeferreds)
  }

  private def translateMundaneKnownSizeArrayLoad(
    hinputs: Hinputs,
    hamuts2: Hamuts,
    locals0: Locals,
    stackHeight0: StackHeight,
    nodesByLine0: Vector[NodeH],
    arrayExpr2: ReferenceExpression2,
    indexExpr2: ReferenceExpression2,
    targetOwnership: Ownership
  ): (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH], List[Expression2]) = {
    val (hamutsH, locals1, stackHeight1, nodesByLine1, Some(arrayResultLine), arrayDeferreds) =
      translate(hinputs, hamuts2, locals0, stackHeight0, nodesByLine0, arrayExpr2);
    val arrayAccess = arrayResultLine.expectKnownSizeArrayAccess()

    val (hamuts4, locals2, stackHeight2, nodesByLine2, Some(indexExprResultLine), indexDeferreds) =
      translate(hinputs, hamutsH, locals1, stackHeight1, nodesByLine1, indexExpr2);
    val indexAccess = indexExprResultLine.expectIntAccess()

    vassert(targetOwnership == Borrow || targetOwnership == Share)

    val borrowedElementType =
      ReferenceH(targetOwnership, arrayAccess.expectedType.kind.rawArray.elementType.kind)

    // We're storing into a regular reference element of an array.
    val (nodesByLineH, loadedNodeH) =
      addNode(
        nodesByLine2,
        KnownSizeArrayLoadH(
          newId(nodesByLine2),
          arrayAccess,
          indexAccess,
          borrowedElementType,
          targetOwnership))
    val loadedAccess =
      RegisterAccessH(loadedNodeH.registerId, borrowedElementType)

    (hamuts4, locals2, stackHeight2, nodesByLineH, loadedAccess, arrayDeferreds ++ indexDeferreds)
  }

  private def translateAddressibleMemberLoad(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      structExpr2: ReferenceExpression2,
      memberName: String,
      expectedType2: Coord,
      targetOwnership: Ownership
  ): (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH], List[Expression2]) = {
    val (hamuts1, locals2, stackHeight2, nodesByLine1, Some(structResultLine), structDeferreds) =
      translate(hinputs, hamuts0, locals1, stackHeight0, nodesByLine0, structExpr2);

    val structRef2 =
      structExpr2.resultRegister.reference.referend match {
        case sr @ StructRef2(_) => sr
        case TupleT2(_, sr) => sr
        case PackT2(_, sr) => sr
      }
    val structDef2 = hinputs.program2.lookupStruct(structRef2)
    val memberIndex = structDef2.members.indexWhere(_.name == memberName)
    vassert(memberIndex >= 0)
    val member2 = structDef2.members(memberIndex)

    val variability = member2.variability

    val boxedType2 = member2.tyype.expectAddressMember().reference

    val (hamuts2, boxedTypeH) =
      TypeHammer.translateReference(hinputs, hamuts1, boxedType2);

    val (hamutsH, boxStructRefH) =
      StructHammer.makeBox(hinputs, hamuts2, variability, boxedType2, boxedTypeH)

    // We expect a borrow because structs never own boxes, they only borrow them
    val expectedStructBoxMemberType = ReferenceH(Borrow, boxStructRefH)
    val expectedBorrowBoxResultType = ReferenceH(Borrow, boxStructRefH)

    // We're storing into a struct's member that is a box. The stack is also
    // pointing at this box. First, get the box, then mutate what's inside.
    val (nodesByLine2, loadBoxNode) =
      addNode(
        nodesByLine1,
        MemberLoadH(
          newId(nodesByLine1),
          structResultLine.expectStructAccess(),
          memberIndex,
          Borrow,
          expectedStructBoxMemberType,
          expectedBorrowBoxResultType,
          memberName))
    val loadBoxAccess =
      RegisterAccessH(loadBoxNode.registerId, expectedBorrowBoxResultType)
    val (nodesByLineH, loadedNodeH) =
      addNode(
        nodesByLine2,
        MemberLoadH(
          newId(nodesByLine2),
          loadBoxAccess,
          StructHammer.BOX_MEMBER_INDEX,
          targetOwnership,
          boxedTypeH,
          ReferenceH(targetOwnership, boxedTypeH.kind),
          StructHammer.BOX_MEMBER_NAME))

    val loadedAccess =
      RegisterAccessH(loadedNodeH.registerId, boxedTypeH)
    (hamutsH, locals2, stackHeight2, nodesByLineH, loadedAccess, structDeferreds)
  }

  private def translateMundaneMemberLoad(
      hinputs: Hinputs,
      hamuts2: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      structExpr2: ReferenceExpression2,
      expectedMemberType2: Coord,
      memberName: String,
      targetOwnership: Ownership
  ): (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH], List[Expression2]) = {
    val (hamutsH, locals2, stackHeight1, nodesByLine1, Some(structResultLine), structDeferreds) =
      translate(hinputs, hamuts2, locals1, stackHeight0, nodesByLine0, structExpr2);

    val (hamuts4, expectedMemberTypeH) =
      TypeHammer.translateReference(hinputs, hamutsH, expectedMemberType2);

    val structRef2 =
      structExpr2.resultRegister.reference.referend match {
        case sr @ StructRef2(_) => sr
        case TupleT2(_, sr) => sr
        case PackT2(_, sr) => sr
      }
    val structDef2 = hinputs.program2.lookupStruct(structRef2)
    val memberIndex = structDef2.members.indexWhere(_.name == memberName)
    vassert(memberIndex >= 0)

    val resultTypeH =
      ReferenceH(targetOwnership, expectedMemberTypeH.kind)

    // We're loading from a regular reference member of a struct.
    val (nodesByLine2, loadedNode) =
      addNode(
        nodesByLine1,
        MemberLoadH(
          newId(nodesByLine1),
          structResultLine.expectStructAccess(),
          memberIndex,
          targetOwnership,
          expectedMemberTypeH,
          resultTypeH,
          memberName))
    val loadedAccess =
      RegisterAccessH(loadedNode.registerId, resultTypeH)
    (hamuts4, locals2, stackHeight1, nodesByLine2, loadedAccess, structDeferreds)
  }

  def translateAddressibleLocalLoad(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine1: Vector[NodeH],
      varId: VariableId2,
      variability: Variability,
      localReference2: Coord,
      targetOwnership: Ownership
  ): (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH], List[Expression2]) = {
    val local = locals1.get(varId).get

    val (hamuts1, localTypeH) =
      TypeHammer.translateReference(hinputs, hamuts0, localReference2);
    val (hamutsH, boxStructRefH) =
      StructHammer.makeBox(hinputs, hamuts1, variability, localReference2, localTypeH)
    vassert(local.typeH.kind == boxStructRefH)

    val expectedStructBoxMemberType = ReferenceH(Own, boxStructRefH)
    val expectedBorrowBoxResultType = ReferenceH(Borrow, boxStructRefH)

    // This means we're trying to load from a local variable that holds a box.
    // We need to load the box, then mutate its contents.
    val (nodesByLine2, loadBoxNode) =
      addNode(
        nodesByLine1,
        LocalLoadH(
          newId(nodesByLine1),
          local,
          Borrow,
          expectedStructBoxMemberType,
          expectedBorrowBoxResultType,
          varId.variableName))
    val loadBoxAccess =
      RegisterAccessH(loadBoxNode.registerId, expectedBorrowBoxResultType)

    val resultTypeH = ReferenceH(targetOwnership, localTypeH.kind)
    val (nodesByLineH, loadedNode) =
      addNode(
        nodesByLine2,
        MemberLoadH(
          newId(nodesByLine2),
          loadBoxAccess,
          StructHammer.BOX_MEMBER_INDEX,
          targetOwnership,
          localTypeH,
          resultTypeH,
          StructHammer.BOX_MEMBER_NAME))
    val loadedAccess =
      RegisterAccessH(loadedNode.registerId, resultTypeH)
    (hamutsH, locals1, stackHeight0, nodesByLineH, loadedAccess, List())
  }

  def translateMundaneLocalLoad(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine1: Vector[NodeH],
      varId: VariableId2,
      expectedType2: Coord,
      targetOwnership: Ownership
  ): (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH], List[Expression2]) = {

    val local = locals1.get(varId) match {
      case Some(x) => x
      case None => {
        vfail("wot")
      }
    }

    val (hamuts1, expectedTypeH) =
      TypeHammer.translateReference(hinputs, hamuts0, expectedType2);
    vassert(expectedTypeH == local.typeH)

    val resultTypeH = ReferenceH(targetOwnership, expectedTypeH.kind)

    val (nodesByLine2, loadedNode) =
      addNode(
        nodesByLine1,
        LocalLoadH(
          newId(nodesByLine1),
          local,
          targetOwnership,
          local.typeH,
          resultTypeH,
          varId.variableName))
    val loadedAccess =
      RegisterAccessH(loadedNode.registerId, resultTypeH)
    (hamuts1, locals1, stackHeight0, nodesByLine2, loadedAccess, List())
  }

  def translateLocalAddress(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine1: Vector[NodeH],
      lookup2: LocalLookup2):
  (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH]) = {
    val LocalLookup2(localVar, type2) = lookup2;
    vassert(type2 == localVar.reference)

    val local = locals1.get(localVar.id).get
    val (hamutsH, boxStructRefH) =
      StructHammer.makeBox(hinputs, hamuts0, localVar.variability, localVar.reference, local.typeH)

    val expectedStructBoxMemberType = ReferenceH(Own, boxStructRefH)
    val expectedBorrowBoxResultType = ReferenceH(Borrow, boxStructRefH)

    // This means we're trying to load from a local variable that holds a box.
    // We need to load the box, then mutate its contents.
    val (nodesByLine2, loadBoxNode) =
    addNode(
      nodesByLine1,
      LocalLoadH(
        newId(nodesByLine1),
        local,
        Borrow,
        expectedStructBoxMemberType,
        expectedBorrowBoxResultType,
        localVar.id.variableName))
    val loadBoxAccess =
      RegisterAccessH(loadBoxNode.registerId, expectedBorrowBoxResultType)
    (hamutsH, locals1, stackHeight0, nodesByLine2, loadBoxAccess)
  }

  def translateMemberAddress(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      lookup2: AddressMemberLookup2):
  (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH], List[Expression2]) = {
    val AddressMemberLookup2(structExpr2, memberName, varId, resultType2) = lookup2;

    val (hamuts1, locals2, stackHeight1, nodesByLine1, Some(structResultLine), structDeferreds) =
      translate(hinputs, hamuts0, locals1, stackHeight0, nodesByLine0, structExpr2);

    val structRef2 =
      structExpr2.resultRegister.reference.referend match {
        case sr @ StructRef2(_) => sr
        case TupleT2(_, sr) => sr
        case PackT2(_, sr) => sr
      }
    val structDef2 = hinputs.program2.lookupStruct(structRef2)
    val memberIndex = structDef2.members.indexWhere(_.name == memberName)
    vassert(memberIndex >= 0)
    val member2 = structDef2.members(memberIndex)

    val variability = member2.variability
    vassert(variability == Varying) // curious

    val boxedType2 = member2.tyype.expectAddressMember().reference

    val (hamuts2, boxedTypeH) =
      TypeHammer.translateReference(hinputs, hamuts1, boxedType2);

    val (hamutsH, boxStructRefH) =
      StructHammer.makeBox(hinputs, hamuts2, variability, boxedType2, boxedTypeH)

    // We expect a borrow because structs never own boxes, they only borrow them
    val expectedStructBoxMemberType = ReferenceH(Borrow, boxStructRefH)
    val expectedBorrowBoxResultType = ReferenceH(Borrow, boxStructRefH)

    // We're storing into a struct's member that is a box. The stack is also
    // pointing at this box. First, get the box, then mutate what's inside.
    val (nodesByLine2, loadBoxNode) =
    addNode(
      nodesByLine1,
      MemberLoadH(
        newId(nodesByLine1),
        structResultLine.expectStructAccess(),
        memberIndex,
        Borrow,
        expectedStructBoxMemberType,
        expectedBorrowBoxResultType,
        memberName))
    val loadBoxAccess =
      RegisterAccessH(loadBoxNode.registerId, expectedBorrowBoxResultType)

    (hamutsH, locals2, stackHeight1, nodesByLine2, loadBoxAccess, structDeferreds)
  }
}
