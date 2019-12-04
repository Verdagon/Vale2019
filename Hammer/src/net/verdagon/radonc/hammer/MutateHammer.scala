package net.verdagon.radonc.hammer

import net.verdagon.radonc.hammer.ExpressionHammer.{addNode, newId, translate, translateDeferreds}
import net.verdagon.radonc.hinputs.Hinputs
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.env.{AddressibleLocalVariable2, ReferenceLocalVariable2, VariableId2}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.vassert

object MutateHammer {

  def translateMutate(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      mutate2: Mutate2):
  (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3]) = {
    val Mutate2(destinationExpr2, sourceExpr2) = mutate2

    val (hamuts1, locals1, stackHeight1, nodesByLine1, Some(sourceExprResultLine), sourceDeferreds) =
      translate(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, sourceExpr2);
    val (hamuts2, sourceResultPointerType3) =
      TypeHammer.translateReference(hinputs, hamuts1, sourceExpr2.resultRegister.reference)

    val (hamuts5, locals5, stackHeight2, nodesByLine5, oldValueAccess, destinationDeferreds) =
      destinationExpr2 match {
        case LocalLookup2(ReferenceLocalVariable2(varId, variability, reference), varType2) => {
          translateMundaneLocalMutate(hamuts2, locals1, stackHeight1, nodesByLine1, sourceExprResultLine, varId)
        }
        case LocalLookup2(AddressibleLocalVariable2(varId, variability, reference), varType2) => {
          translateAddressibleLocalMutate(hinputs, hamuts2, locals1, stackHeight1, nodesByLine1, sourceExprResultLine, sourceResultPointerType3, varId, variability, reference)
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

    val (hamuts6, locals6, stackHeight3, nodesByLine6) =
      translateDeferreds(hinputs, hamuts5, locals5, stackHeight2, nodesByLine5, sourceDeferreds ++ destinationDeferreds)

    (hamuts6, locals6, stackHeight3, nodesByLine6, oldValueAccess)
  }

  private def translateMundaneUnknownSizeArrayMutate(
      hinputs: Hinputs,
      hamuts2: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      sourceExprResultLine: RegisterAccess3[Referend3],
      arrayExpr2: ReferenceExpression2,
      indexExpr2: ReferenceExpression2
  ): (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3], List[Expression2]) = {
    val (hamuts3, locals1, stackHeight1, nodesByLine1, Some(destinationResultLine), destinationDeferreds) =
      translate(hinputs, hamuts2, locals0, stackHeight0, nodesByLine0, arrayExpr2);
    val (hamuts4, locals2, stackHeight2, nodesByLine2, Some(indexExprResultLine), indexDeferreds) =
      translate(hinputs, hamuts3, locals1, stackHeight1, nodesByLine1, indexExpr2);
    // We're storing into a regular reference element of an array.
    val (nodesByLine3, storeNode) =
      addNode(
        nodesByLine2,
        UnknownSizeArrayStore3(
          newId(nodesByLine2),
          destinationResultLine.expectUnknownSizeArrayAccess(),
          indexExprResultLine.expectIntAccess(),
          sourceExprResultLine))

    val oldValueAccess = RegisterAccess3(storeNode.registerId, sourceExprResultLine.expectedType)

    (hamuts4, locals2, stackHeight2, nodesByLine3, oldValueAccess, destinationDeferreds ++ indexDeferreds)
  }

  private def translateMundaneKnownSizeArrayMutate(
    hinputs: Hinputs,
    hamuts2: Hamuts,
    locals0: Locals,
    stackHeight0: StackHeight,
    nodesByLine0: Vector[Node3],
    sourceExprResultLine: RegisterAccess3[Referend3],
    arrayExpr2: ReferenceExpression2,
    indexExpr2: ReferenceExpression2
  ): (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3], List[Expression2]) = {
    val (hamuts3, locals1, stackHeight1, nodesByLine1, Some(destinationResultLine), destinationDeferreds) =
      translate(hinputs, hamuts2, locals0, stackHeight0, nodesByLine0, arrayExpr2);
    val (hamuts4, locals2, stackHeight2, nodesByLine2, Some(indexExprResultLine), indexDeferreds) =
      translate(hinputs, hamuts3, locals1, stackHeight1, nodesByLine1, indexExpr2);
    // We're storing into a regular reference element of an array.
    val (nodesByLine3, storeNode) =
      addNode(
        nodesByLine2,
        KnownSizeArrayStore3(
          newId(nodesByLine2),
          destinationResultLine.expectKnownSizeArrayAccess(),
          indexExprResultLine.expectIntAccess(),
          sourceExprResultLine))

    val oldValueAccess = RegisterAccess3(storeNode.registerId, sourceExprResultLine.expectedType)

    (hamuts4, locals2, stackHeight2, nodesByLine3, oldValueAccess, destinationDeferreds ++ indexDeferreds)
  }

  private def translateAddressibleMemberMutate(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      sourceExprResultLine: RegisterAccess3[Referend3],
      structExpr2: ReferenceExpression2,
      memberName: String
  ): (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3], List[Expression2]) = {
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

    val (hamuts2, boxedType3) =
      TypeHammer.translateReference(hinputs, hamuts1, boxedType2);

    val (hamuts3, boxStructRef3) =
      StructHammer.makeBox(hinputs, hamuts2, variability, boxedType2, boxedType3)

    // Remember, structs can never own boxes, they only borrow them
    val expectedStructBoxMemberType = Reference3(Borrow, boxStructRef3)
    val expectedBorrowBoxResultType = Reference3(Borrow, boxStructRef3)

    // We're storing into a struct's member that is a box. The stack is also
    // pointing at this box. First, get the box, then mutate what's inside.
    val (nodesByLine2, loadBoxNode) =
    addNode(
      nodesByLine1,
      MemberLoad3(
        newId(nodesByLine1),
        destinationResultLine.expectStructAccess(),
        memberIndex,
        Borrow,
        expectedStructBoxMemberType,
        expectedBorrowBoxResultType,
        memberName))
    val loadBoxAccess =
      RegisterAccess3(loadBoxNode.registerId, expectedBorrowBoxResultType)
    val (nodesByLine3, storeNode) =
      addNode(
        nodesByLine2,
        MemberStore3(
          newId(nodesByLine2),
          loadBoxAccess,
          StructHammer.BOX_MEMBER_INDEX,
          sourceExprResultLine,
          StructHammer.BOX_MEMBER_NAME))
    val oldValueAccess = RegisterAccess3(storeNode.registerId, sourceExprResultLine.expectedType)
    (hamuts3, locals2, stackHeight1, nodesByLine3, oldValueAccess, destinationDeferreds)
  }

  private def translateMundaneMemberMutate(
      hinputs: Hinputs,
      hamuts2: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      sourceExprResultLine: RegisterAccess3[Referend3],
      structExpr2: ReferenceExpression2,
      memberName: String
  ): (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3], List[Expression2]) = {
    val (hamuts3, locals2, stackHeight1, nodesByLine1, Some(destinationResultLine), destinationDeferreds) =
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
        MemberStore3(
          newId(nodesByLine1),
          destinationResultLine.expectStructAccess(),
          memberIndex,
          sourceExprResultLine,
          memberName))
    val oldValueAccess = RegisterAccess3(storeNode.registerId, sourceExprResultLine.expectedType)
    (hamuts3, locals2, stackHeight1, nodesByLine2, oldValueAccess, destinationDeferreds)
  }

  private def translateAddressibleLocalMutate(
      hinputs: Hinputs,
      hamuts2: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine1: Vector[Node3],
      sourceExprResultLine: RegisterAccess3[Referend3],
      sourceResultPointerType3: Reference3[Referend3],
      varId: VariableId2,
      variability: Variability,
      reference: Coord
  ): (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3], List[Expression2]) = {
    val local = locals1.get(varId).get
    val (hamuts3, boxStructRef3) =
      StructHammer.makeBox(hinputs, hamuts2, variability, reference, sourceResultPointerType3)
    val expectedLocalBoxType = Reference3(Own, boxStructRef3)
    val expectedBorrowBoxResultType = Reference3(Borrow, boxStructRef3)

    // This means we're trying to mutate a local variable that holds a box.
    // We need to load the box, then mutate its contents.
    val (nodesByLine2, loadBoxNode) =
    addNode(
      nodesByLine1,
      LocalLoad3(
        newId(nodesByLine1),
        local,
        Borrow,
        expectedLocalBoxType,
        expectedBorrowBoxResultType,
        varId.variableName))
    val loadBoxAccess =
      RegisterAccess3(loadBoxNode.registerId, expectedBorrowBoxResultType)
    val (nodesByLine3, storeNode) =
      addNode(
        nodesByLine2,
        MemberStore3(
          newId(nodesByLine2),
          loadBoxAccess,
          StructHammer.BOX_MEMBER_INDEX,
          sourceExprResultLine,
          StructHammer.BOX_MEMBER_NAME))
    val oldValueAccess = RegisterAccess3(storeNode.registerId, sourceExprResultLine.expectedType)
    (hamuts3, locals1, stackHeight0, nodesByLine3, oldValueAccess, List())
  }

  private def translateMundaneLocalMutate(
      hamuts2: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine1: Vector[Node3],
      sourceExprResultLine: RegisterAccess3[Referend3],
      varId: VariableId2
  ): (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3], List[Expression2]) = {
    val local = locals1.get(varId).get
    val (nodesByLine2, newStoreNode) =
      addNode(
        nodesByLine1,
        LocalStore3(
          newId(nodesByLine1),
          local,
          sourceExprResultLine,
          varId.variableName))
    val oldValueAccess = RegisterAccess3(newStoreNode.registerId, sourceExprResultLine.expectedType)
    (hamuts2, locals1, stackHeight0, nodesByLine2, oldValueAccess, List())
  }
}
