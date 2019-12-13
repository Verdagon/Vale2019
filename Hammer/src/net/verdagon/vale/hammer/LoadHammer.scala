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
      nodesByLine0: Vector[Node3],
      load2: SoftLoad2):
  (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3], List[Expression2]) = {
    val SoftLoad2(sourceExpr2, targetOwnership) = load2

    val (hamuts5, locals5, stackHeight5, nodesByLine5, loadedAccess3, sourceDeferreds) =
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

    (hamuts5, locals5, stackHeight5, nodesByLine5, loadedAccess3, sourceDeferreds)
  }

  private def translateMundaneUnknownSizeArrayLoad(
      hinputs: Hinputs,
      hamuts2: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      arrayExpr2: ReferenceExpression2,
      indexExpr2: ReferenceExpression2,
      targetOwnership: Ownership
  ): (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3], List[Expression2]) = {
    val (hamuts3, locals1, stackHeight1, nodesByLine1, Some(arrayResultLine), arrayDeferreds) =
      translate(hinputs, hamuts2, locals0, stackHeight0, nodesByLine0, arrayExpr2);
    val arrayAccess = arrayResultLine.expectUnknownSizeArrayAccess()

    val (hamuts4, locals2, stackHeight2, nodesByLine2, Some(indexExprResultLine), indexDeferreds) =
      translate(hinputs, hamuts3, locals1, stackHeight1, nodesByLine1, indexExpr2);
    val indexAccess = indexExprResultLine.expectIntAccess()

    vassert(targetOwnership == Borrow || targetOwnership == Share)

    val borrowedElementType =
      Reference3(targetOwnership, arrayAccess.expectedType.kind.rawArray.elementType.kind)

    // We're storing into a regular reference element of an array.
    val (nodesByLine3, loadedNode3) =
      addNode(
        nodesByLine2,
        UnknownSizeArrayLoad3(
          newId(nodesByLine2),
          arrayAccess,
          indexAccess,
          borrowedElementType,
          targetOwnership))
    val loadedAccess =
      RegisterAccess3(loadedNode3.registerId, borrowedElementType)

    (hamuts4, locals2, stackHeight2, nodesByLine3, loadedAccess, arrayDeferreds ++ indexDeferreds)
  }

  private def translateMundaneKnownSizeArrayLoad(
    hinputs: Hinputs,
    hamuts2: Hamuts,
    locals0: Locals,
    stackHeight0: StackHeight,
    nodesByLine0: Vector[Node3],
    arrayExpr2: ReferenceExpression2,
    indexExpr2: ReferenceExpression2,
    targetOwnership: Ownership
  ): (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3], List[Expression2]) = {
    val (hamuts3, locals1, stackHeight1, nodesByLine1, Some(arrayResultLine), arrayDeferreds) =
      translate(hinputs, hamuts2, locals0, stackHeight0, nodesByLine0, arrayExpr2);
    val arrayAccess = arrayResultLine.expectKnownSizeArrayAccess()

    val (hamuts4, locals2, stackHeight2, nodesByLine2, Some(indexExprResultLine), indexDeferreds) =
      translate(hinputs, hamuts3, locals1, stackHeight1, nodesByLine1, indexExpr2);
    val indexAccess = indexExprResultLine.expectIntAccess()

    vassert(targetOwnership == Borrow || targetOwnership == Share)

    val borrowedElementType =
      Reference3(targetOwnership, arrayAccess.expectedType.kind.rawArray.elementType.kind)

    // We're storing into a regular reference element of an array.
    val (nodesByLine3, loadedNode3) =
      addNode(
        nodesByLine2,
        KnownSizeArrayLoad3(
          newId(nodesByLine2),
          arrayAccess,
          indexAccess,
          borrowedElementType,
          targetOwnership))
    val loadedAccess =
      RegisterAccess3(loadedNode3.registerId, borrowedElementType)

    (hamuts4, locals2, stackHeight2, nodesByLine3, loadedAccess, arrayDeferreds ++ indexDeferreds)
  }

  private def translateAddressibleMemberLoad(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      structExpr2: ReferenceExpression2,
      memberName: String,
      expectedType2: Coord,
      targetOwnership: Ownership
  ): (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3], List[Expression2]) = {
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

    val (hamuts2, boxedType3) =
      TypeHammer.translateReference(hinputs, hamuts1, boxedType2);

    val (hamuts3, boxStructRef3) =
      StructHammer.makeBox(hinputs, hamuts2, variability, boxedType2, boxedType3)

    // We expect a borrow because structs never own boxes, they only borrow them
    val expectedStructBoxMemberType = Reference3(Borrow, boxStructRef3)
    val expectedBorrowBoxResultType = Reference3(Borrow, boxStructRef3)

    // We're storing into a struct's member that is a box. The stack is also
    // pointing at this box. First, get the box, then mutate what's inside.
    val (nodesByLine2, loadBoxNode) =
      addNode(
        nodesByLine1,
        MemberLoad3(
          newId(nodesByLine1),
          structResultLine.expectStructAccess(),
          memberIndex,
          Borrow,
          expectedStructBoxMemberType,
          expectedBorrowBoxResultType,
          memberName))
    val loadBoxAccess =
      RegisterAccess3(loadBoxNode.registerId, expectedBorrowBoxResultType)
    val (nodesByLine3, loadedNode3) =
      addNode(
        nodesByLine2,
        MemberLoad3(
          newId(nodesByLine2),
          loadBoxAccess,
          StructHammer.BOX_MEMBER_INDEX,
          targetOwnership,
          boxedType3,
          Reference3(targetOwnership, boxedType3.kind),
          StructHammer.BOX_MEMBER_NAME))

    val loadedAccess =
      RegisterAccess3(loadedNode3.registerId, boxedType3)
    (hamuts3, locals2, stackHeight2, nodesByLine3, loadedAccess, structDeferreds)
  }

  private def translateMundaneMemberLoad(
      hinputs: Hinputs,
      hamuts2: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      structExpr2: ReferenceExpression2,
      expectedMemberType2: Coord,
      memberName: String,
      targetOwnership: Ownership
  ): (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3], List[Expression2]) = {
    val (hamuts3, locals2, stackHeight1, nodesByLine1, Some(structResultLine), structDeferreds) =
      translate(hinputs, hamuts2, locals1, stackHeight0, nodesByLine0, structExpr2);

    val (hamuts4, expectedMemberType3) =
      TypeHammer.translateReference(hinputs, hamuts3, expectedMemberType2);

    val structRef2 =
      structExpr2.resultRegister.reference.referend match {
        case sr @ StructRef2(_) => sr
        case TupleT2(_, sr) => sr
        case PackT2(_, sr) => sr
      }
    val structDef2 = hinputs.program2.lookupStruct(structRef2)
    val memberIndex = structDef2.members.indexWhere(_.name == memberName)
    vassert(memberIndex >= 0)

    val resultType3 =
      Reference3(targetOwnership, expectedMemberType3.kind)

    // We're loading from a regular reference member of a struct.
    val (nodesByLine2, loadedNode) =
      addNode(
        nodesByLine1,
        MemberLoad3(
          newId(nodesByLine1),
          structResultLine.expectStructAccess(),
          memberIndex,
          targetOwnership,
          expectedMemberType3,
          resultType3,
          memberName))
    val loadedAccess =
      RegisterAccess3(loadedNode.registerId, resultType3)
    (hamuts4, locals2, stackHeight1, nodesByLine2, loadedAccess, structDeferreds)
  }

  def translateAddressibleLocalLoad(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine1: Vector[Node3],
      varId: VariableId2,
      variability: Variability,
      localReference2: Coord,
      targetOwnership: Ownership
  ): (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3], List[Expression2]) = {
    val local = locals1.get(varId).get

    val (hamuts1, localType3) =
      TypeHammer.translateReference(hinputs, hamuts0, localReference2);
    val (hamuts3, boxStructRef3) =
      StructHammer.makeBox(hinputs, hamuts1, variability, localReference2, localType3)
    vassert(local.type3.kind == boxStructRef3)

    val expectedStructBoxMemberType = Reference3(Own, boxStructRef3)
    val expectedBorrowBoxResultType = Reference3(Borrow, boxStructRef3)

    // This means we're trying to load from a local variable that holds a box.
    // We need to load the box, then mutate its contents.
    val (nodesByLine2, loadBoxNode) =
      addNode(
        nodesByLine1,
        LocalLoad3(
          newId(nodesByLine1),
          local,
          Borrow,
          expectedStructBoxMemberType,
          expectedBorrowBoxResultType,
          varId.variableName))
    val loadBoxAccess =
      RegisterAccess3(loadBoxNode.registerId, expectedBorrowBoxResultType)

    val resultType3 = Reference3(targetOwnership, localType3.kind)
    val (nodesByLine3, loadedNode) =
      addNode(
        nodesByLine2,
        MemberLoad3(
          newId(nodesByLine2),
          loadBoxAccess,
          StructHammer.BOX_MEMBER_INDEX,
          targetOwnership,
          localType3,
          resultType3,
          StructHammer.BOX_MEMBER_NAME))
    val loadedAccess =
      RegisterAccess3(loadedNode.registerId, resultType3)
    (hamuts3, locals1, stackHeight0, nodesByLine3, loadedAccess, List())
  }

  def translateMundaneLocalLoad(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine1: Vector[Node3],
      varId: VariableId2,
      expectedType2: Coord,
      targetOwnership: Ownership
  ): (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3], List[Expression2]) = {

    val local = locals1.get(varId) match {
      case Some(x) => x
      case None => {
        vfail("wot")
      }
    }

    val (hamuts1, expectedType3) =
      TypeHammer.translateReference(hinputs, hamuts0, expectedType2);
    vassert(expectedType3 == local.type3)

    val resultType3 = Reference3(targetOwnership, expectedType3.kind)

    val (nodesByLine2, loadedNode) =
      addNode(
        nodesByLine1,
        LocalLoad3(
          newId(nodesByLine1),
          local,
          targetOwnership,
          local.type3,
          resultType3,
          varId.variableName))
    val loadedAccess =
      RegisterAccess3(loadedNode.registerId, resultType3)
    (hamuts1, locals1, stackHeight0, nodesByLine2, loadedAccess, List())
  }

  def translateLocalAddress(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine1: Vector[Node3],
      lookup2: LocalLookup2):
  (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3]) = {
    val LocalLookup2(localVar, type2) = lookup2;
    vassert(type2 == localVar.reference)

    val local = locals1.get(localVar.id).get
    val (hamuts3, boxStructRef3) =
      StructHammer.makeBox(hinputs, hamuts0, localVar.variability, localVar.reference, local.type3)

    val expectedStructBoxMemberType = Reference3(Own, boxStructRef3)
    val expectedBorrowBoxResultType = Reference3(Borrow, boxStructRef3)

    // This means we're trying to load from a local variable that holds a box.
    // We need to load the box, then mutate its contents.
    val (nodesByLine2, loadBoxNode) =
    addNode(
      nodesByLine1,
      LocalLoad3(
        newId(nodesByLine1),
        local,
        Borrow,
        expectedStructBoxMemberType,
        expectedBorrowBoxResultType,
        localVar.id.variableName))
    val loadBoxAccess =
      RegisterAccess3(loadBoxNode.registerId, expectedBorrowBoxResultType)
    (hamuts3, locals1, stackHeight0, nodesByLine2, loadBoxAccess)
  }

  def translateMemberAddress(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      lookup2: AddressMemberLookup2):
  (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3], List[Expression2]) = {
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

    val (hamuts2, boxedType3) =
      TypeHammer.translateReference(hinputs, hamuts1, boxedType2);

    val (hamuts3, boxStructRef3) =
      StructHammer.makeBox(hinputs, hamuts2, variability, boxedType2, boxedType3)

    // We expect a borrow because structs never own boxes, they only borrow them
    val expectedStructBoxMemberType = Reference3(Borrow, boxStructRef3)
    val expectedBorrowBoxResultType = Reference3(Borrow, boxStructRef3)

    // We're storing into a struct's member that is a box. The stack is also
    // pointing at this box. First, get the box, then mutate what's inside.
    val (nodesByLine2, loadBoxNode) =
    addNode(
      nodesByLine1,
      MemberLoad3(
        newId(nodesByLine1),
        structResultLine.expectStructAccess(),
        memberIndex,
        Borrow,
        expectedStructBoxMemberType,
        expectedBorrowBoxResultType,
        memberName))
    val loadBoxAccess =
      RegisterAccess3(loadBoxNode.registerId, expectedBorrowBoxResultType)

    (hamuts3, locals2, stackHeight1, nodesByLine2, loadBoxAccess, structDeferreds)
  }
}
