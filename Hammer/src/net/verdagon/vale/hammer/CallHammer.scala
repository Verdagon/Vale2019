package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.{metal => m}
import net.verdagon.vale.metal.{Share => _, _}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.templata.{FunctionBanner2, FunctionHeader2, Prototype2}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{vassert, vcurious, vfail}

object CallHammer {

  def translateExternFunctionCall(
    hinputs: Hinputs,
    hamuts: HamutsBox,
    locals: LocalsBox,
    stackHeight: StackHeightBox,
    nodesByLine: NodesBox,
    prototype2: Prototype2,
    argsExprs2: List[ReferenceExpression2]):
  (Option[RegisterAccessH[ReferendH]]) = {
    val (argsResultLines, argsDeferreds) =
      ExpressionHammer.translateExpressions(
        hinputs, hamuts, locals, stackHeight, nodesByLine, argsExprs2);

    // Doublecheck the types
    val (paramTypes) =
      TypeHammer.translateReferences(hinputs, hamuts, prototype2.paramTypes);
    vassert(argsResultLines.map(_.expectedType) == paramTypes)

    val (functionRefH) =
      FunctionHammer.translateFunctionRef(hinputs, hamuts, prototype2);

    val callResultNode =
      nodesByLine.addNode(
        CallH(
          nodesByLine.nextId(),
          functionRefH.prototype,
          argsResultLines))


      ExpressionHammer.translateDeferreds(
        hinputs, hamuts, locals, stackHeight, nodesByLine, argsDeferreds)

    val access =
      if (prototype2.returnType == Coord(Share, Void2())) {
        None
      } else {
        Some(RegisterAccessH(callResultNode.registerId, functionRefH.prototype.returnType))
      }
    access
  }

  def translateFunctionPointerCall(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      function: Prototype2,
      args: List[Expression2],
      resultType2: Coord):
  RegisterAccessH[ReferendH] = {
    val Prototype2(_, paramTypes, returnType2) = function
    val (argLines, argsDeferreds) =
      ExpressionHammer.translateExpressions(
        hinputs, hamuts, locals, stackHeight, nodesByLine, args);

    val prototypeH =
      FunctionHammer.translatePrototype(hinputs, hamuts, function)

    // Doublecheck the types
    val (paramTypesH) =
      TypeHammer.translateReferences(hinputs, hamuts, paramTypes)
    vassert(argLines.map(_.expectedType) == paramTypesH)

    // Doublecheck return
    val (returnTypeH) = TypeHammer.translateReference(hinputs, hamuts, returnType2)
    val (resultTypeH) = TypeHammer.translateReference(hinputs, hamuts, resultType2);
    vassert(returnTypeH == resultTypeH)

    val callResultNode =
      nodesByLine.addNode(
        CallH(
          nodesByLine.nextId(),
          prototypeH,
          argLines));

      ExpressionHammer.translateDeferreds(
        hinputs, hamuts, locals, stackHeight, nodesByLine, argsDeferreds)

    RegisterAccessH(callResultNode.registerId, resultTypeH)
  }

  def translateConstructArray(
      hinputs: Hinputs, hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      constructArray2: ConstructArray2):
  (RegisterAccessH[UnknownSizeArrayTH]) = {
    val ConstructArray2(arrayType2, sizeExpr2, generatorExpr2) = constructArray2;

    val (Some(sizeRegisterId), sizeDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts, locals, stackHeight, nodesByLine, sizeExpr2);

    val (Some(generatorRegisterId), generatorDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts, locals, stackHeight, nodesByLine, generatorExpr2);

    val (arrayRefTypeH) =
      TypeHammer.translateReference(
        hinputs, hamuts, constructArray2.resultRegister.reference)

    val (arrayTypeH) =
      TypeHammer.translateUnknownSizeArray(hinputs, hamuts, arrayType2)
    vassert(arrayRefTypeH.expectUnknownSizeArrayReference().kind == arrayTypeH)

    val constructArrayCallNode =
      nodesByLine.addNode(
        ConstructUnknownSizeArrayH(
          nodesByLine.nextId(),
          sizeRegisterId.expectIntAccess(),
          generatorRegisterId.expectInterfaceAccess(),
          arrayRefTypeH.expectUnknownSizeArrayReference()))


      ExpressionHammer.translateDeferreds(
        hinputs, hamuts, locals, stackHeight, nodesByLine, generatorDeferreds ++ sizeDeferreds)

    val access =
      RegisterAccessH(
        constructArrayCallNode.registerId,
        arrayRefTypeH.expectUnknownSizeArrayReference())
    access
  }

  def translateDestroyArraySequence(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      das2: DestroyArraySequence2):
  Unit = {
    val DestroyArraySequence2(arrayExpr2, arraySequenceType, consumerExpr2) = das2;

    val ArraySequenceT2(size, rawArrayType2 @ RawArrayT2(memberType2, mutability)) = arraySequenceType

    val (arrayTypeH) =
      TypeHammer.translateKnownSizeArray(hinputs, hamuts, arraySequenceType)
    val (arrayRefTypeH) =
      TypeHammer.translateReference(hinputs, hamuts, arrayExpr2.resultRegister.reference)
    vassert(arrayRefTypeH.expectKnownSizeArrayReference().kind == arrayTypeH)

    val (Some(arrayExprResultLine), arrayExprDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts, locals, stackHeight, nodesByLine, arrayExpr2);

    val (Some(consumerCallableResultLine), consumerCallableDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts, locals, stackHeight, nodesByLine, consumerExpr2);

    val destroyArraySequenceCallNode =
      nodesByLine.addNode(
        DestroyKnownSizeArrayH(
          nodesByLine.nextId(),
          arrayExprResultLine.expectKnownSizeArrayAccess(),
          consumerCallableResultLine.expectInterfaceAccess()))


      ExpressionHammer.translateDeferreds(
        hinputs, hamuts, locals, stackHeight, nodesByLine, consumerCallableDeferreds ++ arrayExprDeferreds)
  }

  def translateDestroyUnknownSizeArray(
    hinputs: Hinputs,
    hamuts: HamutsBox,
    locals: LocalsBox,
    stackHeight: StackHeightBox,
    nodesByLine: NodesBox,
    das2: DestroyUnknownSizeArray2):
  Unit = {
    val DestroyUnknownSizeArray2(arrayExpr2, unknownSizeArrayType2, consumerExpr2) = das2;

    val UnknownSizeArrayT2(RawArrayT2(memberType2, mutability)) = unknownSizeArrayType2

    val (arrayTypeH) =
      TypeHammer.translateUnknownSizeArray(hinputs, hamuts, unknownSizeArrayType2)
    val (arrayRefTypeH) =
      TypeHammer.translateReference(hinputs, hamuts, arrayExpr2.resultRegister.reference)
    vassert(arrayRefTypeH.expectUnknownSizeArrayReference().kind == arrayTypeH)

    val (Some(arrayExprResultLine), arrayExprDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts, locals, stackHeight, nodesByLine, arrayExpr2);

    val (Some(consumerCallableResultLine), consumerCallableDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts, locals, stackHeight, nodesByLine, consumerExpr2);

    val destroyArraySequenceCallNode =
      nodesByLine.addNode(
        DestroyUnknownSizeArrayH(
          nodesByLine.nextId(),
          arrayExprResultLine.expectUnknownSizeArrayAccess(),
          consumerCallableResultLine.expectInterfaceAccess()))


      ExpressionHammer.translateDeferreds(
        hinputs, hamuts, locals, stackHeight, nodesByLine, consumerCallableDeferreds ++ arrayExprDeferreds)
  }

  def translateIf(
      hinputs: Hinputs, hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      if2: If2):
  Option[RegisterAccessH[ReferendH]] = {

    val If2(condition2, thenBlock2, elseBlock2) = if2

    val stackHeightForCondition = StackHeightBox(stackHeight.snapshot.oneBlockHigher())
    val (conditionBlockH, maybeConditionResultAccess) =
      BlockHammer.translateBlock(hinputs, hamuts, locals.snapshot, stackHeightForCondition, condition2);
    vassert(maybeConditionResultAccess.get.expectedType == ReferenceH(m.Share, BoolH()))

    val stackHeightForThen = StackHeightBox(stackHeight.snapshot.oneBlockHigher())
    val (thenBlockH, maybeThenResultAccess) =
      BlockHammer.translateBlock(hinputs, hamuts, locals.snapshot, stackHeightForThen, thenBlock2);
    val maybeThenResultCoord = maybeThenResultAccess.map(_.expectedType)

    val stackHeightForElse = StackHeightBox(stackHeight.snapshot.oneBlockHigher())
    val (elseBlockH, maybeElseResultAccess) =
      BlockHammer.translateBlock(hinputs, hamuts, locals.snapshot, stackHeightForElse, elseBlock2);
    val maybeElseResultCoord = maybeElseResultAccess.map(_.expectedType)

    val ifCallNode =
      nodesByLine.addNode(
        IfH(
          nodesByLine.nextId(),
          conditionBlockH,
          thenBlockH,
          elseBlockH))

    val maybeResultCoord =
      (maybeThenResultCoord, maybeElseResultCoord) match {
        case (None, None) => None
        case (Some(ReferenceH(m.Share, NeverH())), Some(ReferenceH(m.Share, NeverH()))) => Some(ReferenceH(m.Share, NeverH()))
        case (Some(ReferenceH(m.Share, NeverH())), None) => None
        case (Some(ReferenceH(m.Share, NeverH())), Some(elseResultCoord)) => Some(elseResultCoord)
        case (None, Some(ReferenceH(m.Share, NeverH()))) => None
        case (Some(thenResultCoord), Some(ReferenceH(m.Share, NeverH()))) => Some(thenResultCoord)
        case (Some(thenResultCoord), Some(elseResultCoord)) => {
          vassert(thenResultCoord == elseResultCoord, "what\n" + maybeThenResultCoord + "\n" + maybeElseResultCoord)
          // Arbitrarily choose the then
          Some(thenResultCoord)
        }
      }
    val maybeResultAccess = maybeResultCoord.map(coord => RegisterAccessH(ifCallNode.registerId, coord))

    (maybeResultAccess)
  }

  def translateWhile(
      hinputs: Hinputs, hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      while2: While2):
  Unit = {

    val While2(bodyExpr2) = while2

    val stackHeightForBody = StackHeightBox(stackHeight.snapshot.oneBlockHigher())
    val (bodyBlockH, maybeBodyResultAccess) =
      BlockHammer.translateBlock(
        hinputs, hamuts, locals.snapshot, stackHeightForBody, bodyExpr2);

    val whileCallNode =
      nodesByLine.addNode(
        WhileH(
          nodesByLine.nextId(),
          bodyBlockH))
  }

  def translateInterfaceFunctionCall(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      superFunctionHeader: FunctionHeader2,
      resultType2: Coord,
      argsExprs2: List[Expression2]):
  RegisterAccessH[ReferendH] = {
    val (argLines, argsDeferreds) =
      ExpressionHammer.translateExpressions(
        hinputs, hamuts, locals, stackHeight, nodesByLine, argsExprs2);

    val virtualParamIndex = superFunctionHeader.getVirtualIndex.get
    val Coord(_, interfaceRef2 @ InterfaceRef2(_)) =
      superFunctionHeader.paramTypes(virtualParamIndex)
    val (interfaceRefH) =
      StructHammer.translateInterfaceRef(hinputs, hamuts, interfaceRef2)
    val edge = hinputs.edgeBlueprintsByInterface(interfaceRef2)
    vassert(edge.interface == interfaceRef2)
    val indexInEdge = edge.superFamilyRootBanners.indexOf(superFunctionHeader.toBanner)
    vassert(indexInEdge >= 0)

    val (prototypeH) = FunctionHammer.translatePrototype(hinputs, hamuts, superFunctionHeader.toPrototype)

    val callNode =
      nodesByLine.addNode(
        InterfaceCallH(
          nodesByLine.nextId(),
          argLines,
          virtualParamIndex,
          interfaceRefH,
          indexInEdge,
          prototypeH));
    val access = RegisterAccessH(callNode.registerId, prototypeH.returnType)

    ExpressionHammer.translateDeferreds(
      hinputs, hamuts, locals, stackHeight, nodesByLine, argsDeferreds)

    (access)
    //
//    val (callResultLine) =
//      superFamilyRootBanner.params.zipWithIndex.collectFirst({
//        case (Parameter2(_, Some(_), Coord(_, interfaceRef2 : InterfaceRef2)), paramIndex) => {
//          val (interfaceRefH) =
//            StructHammer.translateInterfaceRef(hinputs, hamuts, interfaceRef2)
//
//          val (functionNodeLine) =
//            translateInterfaceFunctionCallWithInterface(
//              hinputs,
//              hamuts,
//              nodesByLine,
//              superFamilyRootBanner,
//              paramIndex,
//              interfaceRefH,
//              functionTypeH,
//              argLines)
//          (nodesByLine, functionNodeLine)
//        }
//        case (Parameter2(_, Some(_), Coord(_, structRef2@ StructRef2(_))), _) => {
//          val (functionRegister) =
//            translateInterfaceFunctionLookupWithStruct(
//              hinputs,
//              hamuts,
//              nodesByLine,
//              structRef2,
//              superFamilyRootBanner)
//          val callResultNode =
//            addNode(
//              nodesByLine,
//              CallH(
//                nodesByLine.nextId(),
//                functionRegister,
//                argLines));
//
//          val returnType2 = functionRegister.expectedType.expectFunctionReference().innerType.returnType
//          val access =
//            if (returnType2 == ReferenceH(m.Share, VoidH())) {
//              None
//            } else {
//              Some(RegisterAccessH(callResultNode.registerId, returnType2))
//            }
//          (nodesByLine, access)
//        }
//      }).get
//
//
//      ExpressionHammer.translateDeferreds(
//        hinputs, hamuts, locals, stackHeight, nodesByLine, argsDeferreds)
//    (callResultLine)
  }
//
//  private def translateInterfaceFunctionLookupWithStruct(
//      hinputs: Hinputs,
//      hamuts: HamutsBox,
//      nodesByLine: NodesBox,
//      structRef2: StructRef2,
//      superFamilyRootBanner: FunctionBanner2):
//  (Vector[NodeH], RegisterAccessH[FunctionTH]) = {
//    val prototype2 =
//      getPrototypeForStructInterfaceCall(hinputs, structRef2, superFamilyRootBanner)
//
//    val (functionRefH) =
//      FunctionHammer.translateFunctionRef(hinputs, hamuts, prototype2);
//    val functionNode =
//      addNode(
//        nodesByLine,
//        LoadFunctionH(nodesByLine.nextId(), functionRefH));
//    val access = RegisterAccessH(functionNode.registerId, ReferenceH(m.Raw, functionRefH.functionType))
//    (nodesByLine, access)
//  }
//
//  private def translateInterfaceFunctionCallWithInterface(
//      hinputs: Hinputs,
//      hamuts: HamutsBox,
//      nodesByLine: NodesBox,
//      superFamilyRootBanner: FunctionBanner2,
//      firstVirtualParamIndex: Int,
//      firstVirtualParamInterface: InterfaceRefH,
//      functionTypeH: FunctionTH,
//      argLines: List[RegisterAccessH[ReferendH]]):
//  (Vector[NodeH], Option[RegisterAccessH[ReferendH]]) = {
//    val interfaceId = firstVirtualParamInterface.interfaceId
//
//    val edgeBlueprint =
//      hinputs.edgeBlueprintsByInterfaceId(interfaceId)
//    val indexInEdge =
//      edgeBlueprint.superFamilyRootBanners.indexOf(superFamilyRootBanner)
//    if (indexInEdge < 0) {
//      vfail("Can't find:\n" + superFamilyRootBanner + "\nin:\n" + edgeBlueprint.interface)
//    }
//
//    val methodNode =
//      addNode(
//        nodesByLine,
//        InterfaceCallH(
//          nodesByLine.nextId(),
//          argLines,
//          firstVirtualParamIndex,
//          firstVirtualParamInterface,
//          interfaceId,
//          indexInEdge,
//          functionTypeH));
//
//    val access =
//      if (functionTypeH.returnType == ReferenceH(m.Share, VoidH())) {
//        None
//      } else {
//        Some(RegisterAccessH(methodNode.registerId, functionTypeH.returnType))
//      }
//    (nodesByLine, access)
//  }

  private def getPrototypeForStructInterfaceCall(
      hinputs: Hinputs,
      structRef2: StructRef2,
      superFamilyRootBanner: FunctionBanner2):
  Prototype2 = {

    val structDef2 = hinputs.program2.lookupStruct(structRef2)
    val ancestorInterfaces2 =
      hinputs.program2.impls.filter(impl => impl.struct == structDef2.getRef).map(_.interface)
    val edgeBlueprints = ancestorInterfaces2.map(hinputs.edgeBlueprintsByInterface)
    val matchingEdgeBlueprint =
      edgeBlueprints.find(_.superFamilyRootBanners.contains(superFamilyRootBanner)).get;

    val indexInEdgeBlueprint = matchingEdgeBlueprint.superFamilyRootBanners.indexOf(superFamilyRootBanner);
    vassert(indexInEdgeBlueprint >= 0);

    val edge =
      hinputs.edges.find(
        edge => edge.interface == matchingEdgeBlueprint.interface && edge.struct == structRef2).get;
    val methodPrototype2 = edge.methods(indexInEdgeBlueprint)
    methodPrototype2
  }

}
