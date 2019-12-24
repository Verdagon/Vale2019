package net.verdagon.vale.hammer

import net.verdagon.vale.hammer.ExpressionHammer.{addNode, newId, translate, translateDeferreds}
import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.{AddressibleLocalVariable2, ReferenceLocalVariable2, VariableId2}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.vassert

object MutateHammer {

  def translateMutate(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      mutate2: Mutate2):
  (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH]) = {
    val Mutate2(destinationExpr2, sourceExpr2) = mutate2

    val (hamuts1, locals1, stackHeight1, nodesByLine1, Some(sourceExprResultLine), sourceDeferreds) =
      translate(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, sourceExpr2);
    val (hamuts2, sourceResultPointerTypeH) =
      TypeHammer.translateReference(hinputs, hamuts1, sourceExpr2.resultRegister.reference)

    val (hamuts5, locals5, stackHeight2, nodesByLine5, oldValueAccess, destinationDeferreds) =
      destinationExpr2 match {
        case LocalLookup2(ReferenceLocalVariable2(varId, variability, reference), varType2) => {
          translateMundaneLocalMutate(hamuts2, locals1, stackHeight1, nodesByLine1, sourceExprResultLine, varId)
        }
        case LocalLookup2(AddressibleLocalVariable2(varId, variability, reference), varType2) => {
          translateAddressibleLocalMutate(hinputs, hamuts2, locals1, stackHeight1, nodesByLine1, sourceExprResultLine, sourceResultPointerTypeH, varId, variability, reference)
        }
        case ReferenceMemberLookup2(structExpr2, memberName, memberType2) => {
          translateMundaneMemberMutate(hinputs, hamuts2, locals1, stackHeight1, nodesByLine1, sourceExprResultLine, structExpr2, memberName)
        }
        case AddressMemberLookup2(structExpr2, memberName, varName, memberType2) => {
          translateAddressibleMemberMutate(hinputs, hamuts2, locals1, stackHeight1, nodesByLine1, sourceExprResultLine, structExpr2, memberName)
        }
        case ArraySequenceLookup2(arrayExpr2, arrayType, indexExpr2) => {
          translateMundaneKnownSizeArrayMutate(hinputs, hamuts2, locals1, stackHeight1, nodesByLine1, sourceExprResultLine, arrayExpr2, indexExpr2)
        }
        case UnknownSizeArrayLookup2(arrayExpr2, arrayType, indexExpr2) => {
          translateMundaneUnknownSizeArrayMutate(hinputs, hamuts2, locals1, stackHeight1, nodesByLine1, sourceExprResultLine, arrayExpr2, indexExpr2)
        }
      }

    val (hamuts6, locals6, stackHeightH, nodesByLine6) =
      translateDeferreds(hinputs, hamuts5, locals5, stackHeight2, nodesByLine5, sourceDeferreds ++ destinationDeferreds)

    (hamuts6, locals6, stackHeightH, nodesByLine6, oldValueAccess)
  }

  private def translateMundaneUnknownSizeArrayMutate(
      hinputs: Hinputs,
      hamuts2: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      sourceExprResultLine: RegisterAccessH[ReferendH],
      arrayExpr2: ReferenceExpression2,
      indexExpr2: ReferenceExpression2
  ): (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH], List[Expression2]) = {
    val (hamutsH, locals1, stackHeight1, nodesByLine1, Some(destinationResultLine), destinationDeferreds) =
      translate(hinputs, hamuts2, locals0, stackHeight0, nodesByLine0, arrayExpr2);
    val (hamuts4, locals2, stackHeight2, nodesByLine2, Some(indexExprResultLine), indexDeferreds) =
      translate(hinputs, hamutsH, locals1, stackHeight1, nodesByLine1, indexExpr2);
    // We're storing into a regular reference element of an array.
    val (nodesByLineH, storeNode) =
      addNode(
        nodesByLine2,
        UnknownSizeArrayStoreH(
          newId(nodesByLine2),
          destinationResultLine.expectUnknownSizeArrayAccess(),
          indexExprResultLine.expectIntAccess(),
          sourceExprResultLine))

    val oldValueAccess = RegisterAccessH(storeNode.registerId, sourceExprResultLine.expectedType)

    (hamuts4, locals2, stackHeight2, nodesByLineH, oldValueAccess, destinationDeferreds ++ indexDeferreds)
  }

  private def translateMundaneKnownSizeArrayMutate(
    hinputs: Hinputs,
    hamuts2: Hamuts,
    locals0: Locals,
    stackHeight0: StackHeight,
    nodesByLine0: Vector[NodeH],
    sourceExprResultLine: RegisterAccessH[ReferendH],
    arrayExpr2: ReferenceExpression2,
    indexExpr2: ReferenceExpression2
  ): (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH], List[Expression2]) = {
    val (hamutsH, locals1, stackHeight1, nodesByLine1, Some(destinationResultLine), destinationDeferreds) =
      translate(hinputs, hamuts2, locals0, stackHeight0, nodesByLine0, arrayExpr2);
    val (hamuts4, locals2, stackHeight2, nodesByLine2, Some(indexExprResultLine), indexDeferreds) =
      translate(hinputs, hamutsH, locals1, stackHeight1, nodesByLine1, indexExpr2);
    // We're storing into a regular reference element of an array.
    val (nodesByLineH, storeNode) =
      addNode(
        nodesByLine2,
        KnownSizeArrayStoreH(
          newId(nodesByLine2),
          destinationResultLine.expectKnownSizeArrayAccess(),
          indexExprResultLine.expectIntAccess(),
          sourceExprResultLine))

    val oldValueAccess = RegisterAccessH(storeNode.registerId, sourceExprResultLine.expectedType)

    (hamuts4, locals2, stackHeight2, nodesByLineH, oldValueAccess, destinationDeferreds ++ indexDeferreds)
  }

  private def translateAddressibleMemberMutate(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      sourceExprResultLine: RegisterAccessH[ReferendH],
      structExpr2: ReferenceExpression2,
      memberName: String
  ): (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH], List[Expression2]) = {
    val (hamuts1, locals2, stackHeight1, nodesByLine1, Some(destinationResultLine), destinationDeferreds) =
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

    // Remember, structs can never own boxes, they only borrow them
    val expectedStructBoxMemberType = ReferenceH(Borrow, boxStructRefH)
    val expectedBorrowBoxResultType = ReferenceH(Borrow, boxStructRefH)

    // We're storing into a struct's member that is a box. The stack is also
    // pointing at this box. First, get the box, then mutate what's inside.
    val (nodesByLine2, loadBoxNode) =
    addNode(
      nodesByLine1,
      MemberLoadH(
        newId(nodesByLine1),
        destinationResultLine.expectStructAccess(),
        memberIndex,
        Borrow,
        expectedStructBoxMemberType,
        expectedBorrowBoxResultType,
        memberName))
    val loadBoxAccess =
      RegisterAccessH(loadBoxNode.registerId, expectedBorrowBoxResultType)
    val (nodesByLineH, storeNode) =
      addNode(
        nodesByLine2,
        MemberStoreH(
          newId(nodesByLine2),
          loadBoxAccess,
          StructHammer.BOX_MEMBER_INDEX,
          sourceExprResultLine,
          StructHammer.BOX_MEMBER_NAME))
    val oldValueAccess = RegisterAccessH(storeNode.registerId, sourceExprResultLine.expectedType)
    (hamutsH, locals2, stackHeight1, nodesByLineH, oldValueAccess, destinationDeferreds)
  }

  private def translateMundaneMemberMutate(
      hinputs: Hinputs,
      hamuts2: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      sourceExprResultLine: RegisterAccessH[ReferendH],
      structExpr2: ReferenceExpression2,
      memberName: String
  ): (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH], List[Expression2]) = {
    val (hamutsH, locals2, stackHeight1, nodesByLine1, Some(destinationResultLine), destinationDeferreds) =
      translate(hinputs, hamuts2, locals1, stackHeight0, nodesByLine0, structExpr2);

    val structRef2 =
      structExpr2.resultRegister.reference.referend match {
        case sr @ StructRef2(_) => sr
      }
    val structDef2 = hinputs.program2.lookupStruct(structRef2)
    val memberIndex = structDef2.members.indexWhere(_.name == memberName)
    vassert(memberIndex >= 0)

    // We're storing into a regular reference member of a struct.
    val (nodesByLine2, storeNode) =
      addNode(
        nodesByLine1,
        MemberStoreH(
          newId(nodesByLine1),
          destinationResultLine.expectStructAccess(),
          memberIndex,
          sourceExprResultLine,
          memberName))
    val oldValueAccess = RegisterAccessH(storeNode.registerId, sourceExprResultLine.expectedType)
    (hamutsH, locals2, stackHeight1, nodesByLine2, oldValueAccess, destinationDeferreds)
  }

  private def translateAddressibleLocalMutate(
      hinputs: Hinputs,
      hamuts2: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine1: Vector[NodeH],
      sourceExprResultLine: RegisterAccessH[ReferendH],
      sourceResultPointerTypeH: ReferenceH[ReferendH],
      varId: VariableId2,
      variability: Variability,
      reference: Coord
  ): (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH], List[Expression2]) = {
    val local = locals1.get(varId).get
    val (hamutsH, boxStructRefH) =
      StructHammer.makeBox(hinputs, hamuts2, variability, reference, sourceResultPointerTypeH)
    val expectedLocalBoxType = ReferenceH(Own, boxStructRefH)
    val expectedBorrowBoxResultType = ReferenceH(Borrow, boxStructRefH)

    // This means we're trying to mutate a local variable that holds a box.
    // We need to load the box, then mutate its contents.
    val (nodesByLine2, loadBoxNode) =
    addNode(
      nodesByLine1,
      LocalLoadH(
        newId(nodesByLine1),
        local,
        Borrow,
        expectedLocalBoxType,
        expectedBorrowBoxResultType,
        varId.variableName))
    val loadBoxAccess =
      RegisterAccessH(loadBoxNode.registerId, expectedBorrowBoxResultType)
    val (nodesByLineH, storeNode) =
      addNode(
        nodesByLine2,
        MemberStoreH(
          newId(nodesByLine2),
          loadBoxAccess,
          StructHammer.BOX_MEMBER_INDEX,
          sourceExprResultLine,
          StructHammer.BOX_MEMBER_NAME))
    val oldValueAccess = RegisterAccessH(storeNode.registerId, sourceExprResultLine.expectedType)
    (hamutsH, locals1, stackHeight0, nodesByLineH, oldValueAccess, List())
  }

  private def translateMundaneLocalMutate(
      hamuts2: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine1: Vector[NodeH],
      sourceExprResultLine: RegisterAccessH[ReferendH],
      varId: VariableId2
  ): (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH], List[Expression2]) = {
    val local = locals1.get(varId).get
    val (nodesByLine2, newStoreNode) =
      addNode(
        nodesByLine1,
        LocalStoreH(
          newId(nodesByLine1),
          local,
          sourceExprResultLine,
          varId.variableName))
    val oldValueAccess = RegisterAccessH(newStoreNode.registerId, sourceExprResultLine.expectedType)
    (hamuts2, locals1, stackHeight0, nodesByLine2, oldValueAccess, List())
  }
}
