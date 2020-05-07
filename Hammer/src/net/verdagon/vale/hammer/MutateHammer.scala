package net.verdagon.vale.hammer

import net.verdagon.vale.hammer.ExpressionHammer.{ translate, translateDeferreds}
import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.metal.{Borrow => _, Variability => _, _}
import net.verdagon.vale.{metal => m}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.{AddressibleLocalVariable2, ReferenceLocalVariable2}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.vassert

object MutateHammer {

  def translateMutate(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      mutate2: Mutate2):
  (RegisterAccessH[ReferendH]) = {
    val Mutate2(destinationExpr2, sourceExpr2) = mutate2

    val (Some(sourceExprResultLine), sourceDeferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, sourceExpr2);
    val (sourceResultPointerTypeH) =
      TypeHammer.translateReference(hinputs, hamuts, sourceExpr2.resultRegister.reference)

    val (oldValueAccess, destinationDeferreds) =
      destinationExpr2 match {
        case LocalLookup2(ReferenceLocalVariable2(varId, variability, reference), varType2) => {
          translateMundaneLocalMutate(hamuts, locals, stackHeight, nodesByLine, sourceExprResultLine, varId)
        }
        case LocalLookup2(AddressibleLocalVariable2(varId, variability, reference), varType2) => {
          translateAddressibleLocalMutate(hinputs, hamuts, locals, stackHeight, nodesByLine, sourceExprResultLine, sourceResultPointerTypeH, varId, variability, reference)
        }
        case ReferenceMemberLookup2(structExpr2, memberName, memberType2) => {
          translateMundaneMemberMutate(hinputs, hamuts, locals, stackHeight, nodesByLine, sourceExprResultLine, structExpr2, memberName)
        }
        case AddressMemberLookup2(structExpr2, memberName, memberType2) => {
          translateAddressibleMemberMutate(hinputs, hamuts, locals, stackHeight, nodesByLine, sourceExprResultLine, structExpr2, memberName)
        }
        case ArraySequenceLookup2(arrayExpr2, arrayType, indexExpr2) => {
          translateMundaneKnownSizeArrayMutate(hinputs, hamuts, locals, stackHeight, nodesByLine, sourceExprResultLine, arrayExpr2, indexExpr2)
        }
        case UnknownSizeArrayLookup2(arrayExpr2, arrayType, indexExpr2) => {
          translateMundaneUnknownSizeArrayMutate(hinputs, hamuts, locals, stackHeight, nodesByLine, sourceExprResultLine, arrayExpr2, indexExpr2)
        }
      }


      translateDeferreds(hinputs, hamuts, locals, stackHeight, nodesByLine, sourceDeferreds ++ destinationDeferreds)

    (oldValueAccess)
  }

  private def translateMundaneUnknownSizeArrayMutate(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      sourceExprResultLine: RegisterAccessH[ReferendH],
      arrayExpr2: ReferenceExpression2,
      indexExpr2: ReferenceExpression2
  ): (RegisterAccessH[ReferendH], List[Expression2]) = {
    val (Some(destinationResultLine), destinationDeferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, arrayExpr2);
    val (Some(indexExprResultLine), indexDeferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, indexExpr2);
    // We're storing into a regular reference element of an array.
    val storeNode =
      nodesByLine.addNode(
        UnknownSizeArrayStoreH(
          nodesByLine.nextId(),
          destinationResultLine.expectUnknownSizeArrayAccess(),
          indexExprResultLine.expectIntAccess(),
          sourceExprResultLine))

    val oldValueAccess = RegisterAccessH(storeNode.registerId, sourceExprResultLine.expectedType)

    (oldValueAccess, destinationDeferreds ++ indexDeferreds)
  }

  private def translateMundaneKnownSizeArrayMutate(
    hinputs: Hinputs,
    hamuts: HamutsBox,
    locals: LocalsBox,
    stackHeight: StackHeightBox,
    nodesByLine: NodesBox,
    sourceExprResultLine: RegisterAccessH[ReferendH],
    arrayExpr2: ReferenceExpression2,
    indexExpr2: ReferenceExpression2
  ): (RegisterAccessH[ReferendH], List[Expression2]) = {
    val (Some(destinationResultLine), destinationDeferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, arrayExpr2);
    val (Some(indexExprResultLine), indexDeferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, indexExpr2);
    // We're storing into a regular reference element of an array.
    val storeNode =
      nodesByLine.addNode(
        KnownSizeArrayStoreH(
          nodesByLine.nextId(),
          destinationResultLine.expectKnownSizeArrayAccess(),
          indexExprResultLine.expectIntAccess(),
          sourceExprResultLine))

    val oldValueAccess = RegisterAccessH(storeNode.registerId, sourceExprResultLine.expectedType)

    (oldValueAccess, destinationDeferreds ++ indexDeferreds)
  }

  private def translateAddressibleMemberMutate(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      sourceExprResultLine: RegisterAccessH[ReferendH],
      structExpr2: ReferenceExpression2,
      memberName: FullName2[IVarName2]
  ): (RegisterAccessH[ReferendH], List[Expression2]) = {
    val (Some(destinationResultLine), destinationDeferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, structExpr2);

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

    val (boxedTypeH) =
      TypeHammer.translateReference(hinputs, hamuts, boxedType2);

    val (boxStructRefH) =
      StructHammer.makeBox(hinputs, hamuts, variability, boxedType2, boxedTypeH)

    // Remember, structs can never own boxes, they only borrow them
    val expectedStructBoxMemberType = ReferenceH(m.Borrow, boxStructRefH)
    val expectedBorrowBoxResultType = ReferenceH(m.Borrow, boxStructRefH)

    // We're storing into a struct's member that is a box. The stack is also
    // pointing at this box. First, get the box, then mutate what's inside.
    val loadBoxNode =
    nodesByLine.addNode(
      MemberLoadH(
        nodesByLine.nextId(),
        destinationResultLine.expectStructAccess(),
        memberIndex,
        m.Borrow,
        expectedStructBoxMemberType,
        expectedBorrowBoxResultType,
        NameHammer.stringify(memberName)))
    val loadBoxAccess =
      RegisterAccessH(loadBoxNode.registerId, expectedBorrowBoxResultType)
    val storeNode =
      nodesByLine.addNode(
        MemberStoreH(
          nodesByLine.nextId(),
          loadBoxAccess,
          StructHammer.BOX_MEMBER_INDEX,
          sourceExprResultLine,
          StructHammer.BOX_MEMBER_NAME))
    val oldValueAccess = RegisterAccessH(storeNode.registerId, sourceExprResultLine.expectedType)
    (oldValueAccess, destinationDeferreds)
  }

  private def translateMundaneMemberMutate(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      sourceExprResultLine: RegisterAccessH[ReferendH],
      structExpr2: ReferenceExpression2,
      memberName: FullName2[IVarName2]
  ): (RegisterAccessH[ReferendH], List[Expression2]) = {
    val (Some(destinationResultLine), destinationDeferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, structExpr2);

    val structRef2 =
      structExpr2.resultRegister.reference.referend match {
        case sr @ StructRef2(_) => sr
      }
    val structDef2 = hinputs.program2.lookupStruct(structRef2)
    val memberIndex = structDef2.members.indexWhere(_.name == memberName)
    vassert(memberIndex >= 0)

    // We're storing into a regular reference member of a struct.
    val storeNode =
      nodesByLine.addNode(
        MemberStoreH(
          nodesByLine.nextId(),
          destinationResultLine.expectStructAccess(),
          memberIndex,
          sourceExprResultLine,
          NameHammer.stringify(memberName)))
    val oldValueAccess = RegisterAccessH(storeNode.registerId, sourceExprResultLine.expectedType)
    (oldValueAccess, destinationDeferreds)
  }

  private def translateAddressibleLocalMutate(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      sourceExprResultLine: RegisterAccessH[ReferendH],
      sourceResultPointerTypeH: ReferenceH[ReferendH],
      varId: FullName2[IVarName2],
      variability: Variability,
      reference: Coord
  ): (RegisterAccessH[ReferendH], List[Expression2]) = {
    val local = locals.get(varId).get
    val (boxStructRefH) =
      StructHammer.makeBox(hinputs, hamuts, variability, reference, sourceResultPointerTypeH)
    val expectedLocalBoxType = ReferenceH(m.Own, boxStructRefH)
    val expectedBorrowBoxResultType = ReferenceH(m.Borrow, boxStructRefH)

    // This means we're trying to mutate a local variable that holds a box.
    // We need to load the box, then mutate its contents.
    val loadBoxNode =
    nodesByLine.addNode(
      LocalLoadH(
        nodesByLine.nextId(),
        local,
        m.Borrow,
        expectedLocalBoxType,
        expectedBorrowBoxResultType,
        NameHammer.stringify(varId)))
    val loadBoxAccess =
      RegisterAccessH(loadBoxNode.registerId, expectedBorrowBoxResultType)
    val storeNode =
      nodesByLine.addNode(
        MemberStoreH(
          nodesByLine.nextId(),
          loadBoxAccess,
          StructHammer.BOX_MEMBER_INDEX,
          sourceExprResultLine,
          StructHammer.BOX_MEMBER_NAME))
    val oldValueAccess = RegisterAccessH(storeNode.registerId, sourceExprResultLine.expectedType)
    (oldValueAccess, List())
  }

  private def translateMundaneLocalMutate(
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      sourceExprResultLine: RegisterAccessH[ReferendH],
      varId: FullName2[IVarName2]
  ): (RegisterAccessH[ReferendH], List[Expression2]) = {
    val local = locals.get(varId).get
    val newStoreNode =
      nodesByLine.addNode(
        LocalStoreH(
          nodesByLine.nextId(),
          local,
          sourceExprResultLine,
          NameHammer.stringify(varId)))
    val oldValueAccess = RegisterAccessH(newStoreNode.registerId, sourceExprResultLine.expectedType)
    (oldValueAccess, List())
  }
}
