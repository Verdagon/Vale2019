package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.{metal => m}
import net.verdagon.vale.metal.{Raw => _, _}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.templata.{FunctionBanner2, FunctionHeader2, Prototype2}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{vassert, vcurious, vfail}

object CallHammer {

  def newId(nodesByLine: Vector[NodeH]) = Hammer.newId(nodesByLine)

  def addLine(nodesByLine: Vector[NodeH], node: NodeH): (Vector[NodeH], NodeH) = {
    (nodesByLine :+ node, node)
  }

  def translateExternFunctionCall(
    hinputs: Hinputs,
    hamuts0: Hamuts,
    locals0: Locals,
    stackHeight0: StackHeight,
    nodesByLine0: Vector[NodeH],
    prototype2: Prototype2,
    argsExprs2: List[ReferenceExpression2]):
  (Hamuts, Locals, StackHeight, Vector[NodeH], Option[RegisterAccessH[ReferendH]]) = {
    val (hamuts1, locals1, stackHeight1, nodesByLine1, argsResultLines, argsDeferreds) =
      ExpressionHammer.translateExpressions(
        hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, argsExprs2);

    // Doublecheck the types
    val (hamuts2, paramTypes) =
      TypeHammer.translateReferences(hinputs, hamuts1, prototype2.paramTypes);
    vassert(argsResultLines.map(_.expectedType) == paramTypes)

    val (hamuts3, functionRefH) =
      FunctionHammer.translateFunctionRef(hinputs, hamuts2, prototype2);

    val (nodesByLine3, callResultNode) =
      addLine(
        nodesByLine1,
        CallH(
          newId(nodesByLine1),
          functionRefH.prototype,
          argsResultLines))

    val (hamuts5, locals2, stackHeight2, nodesByLine4) =
      ExpressionHammer.translateDeferreds(
        hinputs, hamuts3, locals1, stackHeight1, nodesByLine3, argsDeferreds)

    val access =
      if (prototype2.returnType == Coord(Raw, Void2())) {
        None
      } else {
        Some(RegisterAccessH(callResultNode.registerId, functionRefH.prototype.returnType))
      }
    (hamuts5, locals2, stackHeight2, nodesByLine4, access)
  }

  def translateFunctionPointerCall(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      function: Prototype2,
      args: List[Expression2],
      resultType2: Coord):
  (Hamuts, Locals, StackHeight, Vector[NodeH], Option[RegisterAccessH[ReferendH]]) = {
    vcurious()
//    val Coord(Raw, FunctionT2(paramTypes, returnType2)) = callableExpr.resultRegister.reference
//    val (hamuts2, locals1, stackHeight1, nodesByLine1, argLines, argsDeferreds) =
//      ExpressionHammer.translateExpressions(
//        hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, args);
//
//    val (hamuts3, locals2, stackHeight2, nodesByLine2, Some(functionLine), callableDeferreds) =
//      ExpressionHammer.translate(hinputs, hamuts2, locals1, stackHeight1,  nodesByLine1, callableExpr)
//
//    // Doublecheck the types
//    val (hamuts4, paramTypesH) =
//      TypeHammer.translateReferences(hinputs, hamuts3, paramTypes)
//    vassert(argLines.map(_.expectedType) == paramTypesH)
//
//    // Doublecheck return
//    val (hamuts5, returnTypeH) = TypeHammer.translateReference(hinputs, hamuts4, returnType2)
//    val (hamuts6, resultTypeH) = TypeHammer.translateReference(hinputs, hamuts5, resultType2);
//    vassert(returnTypeH == resultTypeH)
//
//    val (nodesByLine3, callResultNode) =
//      addLine(
//        nodesByLine2,
//        CallH(
//          newId(nodesByLine2),
//          functionLine.expectFunctionAccess(),
//          argLines));
//
//    val (hamuts7, locals3, stackHeight3, nodesByLine4) =
//      ExpressionHammer.translateDeferreds(
//        hinputs, hamuts6, locals2, stackHeight2, nodesByLine3, argsDeferreds ++ callableDeferreds)
//
//    val access =
//      if (returnType2 == Coord(Raw, Void2())) {
//        None
//      } else {
//        Some(RegisterAccessH(callResultNode.registerId, resultTypeH))
//      }
//    (hamuts7, locals3, stackHeight3, nodesByLine4, access)
  }

  def translateConstructArray(
      hinputs: Hinputs, hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      constructArray2: ConstructArray2):
  (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[UnknownSizeArrayTH]) = {
    val ConstructArray2(arrayType2, sizeExpr2, generatorExpr2) = constructArray2;

    val (hamuts1, locals1, stackHeight1, nodesByLine1, Some(sizeRegisterId), sizeDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, sizeExpr2);

    val (hamuts2, locals2, stackHeight2, nodesByLine2, Some(generatorRegisterId), generatorDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts1, locals1, stackHeight1, nodesByLine1, generatorExpr2);

    val (hamuts6, arrayRefTypeH) =
      TypeHammer.translateReference(
        hinputs, hamuts2, constructArray2.resultRegister.reference)

    val (hamuts7, arrayTypeH) =
      TypeHammer.translateUnknownSizeArray(hinputs, hamuts6, arrayType2)
    vassert(arrayRefTypeH.expectUnknownSizeArrayReference().kind == arrayTypeH)

    val (nodesByLine10, constructArrayCallNode) =
      addLine(nodesByLine2,
        ConstructUnknownSizeArrayH(
          newId(nodesByLine2),
          sizeRegisterId.expectIntAccess(),
          generatorRegisterId.expectInterfaceAccess(),
          arrayRefTypeH.expectUnknownSizeArrayReference()))

    val (hamuts8, locals4, stackHeight4, nodesByLine11) =
      ExpressionHammer.translateDeferreds(
        hinputs, hamuts7, locals2, stackHeight2, nodesByLine10, generatorDeferreds ++ sizeDeferreds)

    val access =
      RegisterAccessH(
        constructArrayCallNode.registerId,
        arrayRefTypeH.expectUnknownSizeArrayReference())
    (hamuts8, locals4, stackHeight4, nodesByLine11, access)
  }

  def translateDestroyArraySequence(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      das2: DestroyArraySequence2):
  (Hamuts, Locals, StackHeight, Vector[NodeH]) = {
    val DestroyArraySequence2(arrayExpr2, arraySequenceType, consumerExpr2) = das2;

    val ArraySequenceT2(size, rawArrayType2 @ RawArrayT2(memberType2, mutability)) = arraySequenceType

    val (hamuts1, arrayTypeH) =
      TypeHammer.translateKnownSizeArray(hinputs, hamuts0, arraySequenceType)
    val (hamuts2, arrayRefTypeH) =
      TypeHammer.translateReference(hinputs, hamuts1, arrayExpr2.resultRegister.reference)
    vassert(arrayRefTypeH.expectKnownSizeArrayReference().kind == arrayTypeH)

    val (hamuts3, locals1, stackHeight1, nodesByLine1, Some(arrayExprResultLine), arrayExprDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts2, locals0, stackHeight0, nodesByLine0, arrayExpr2);

    val (hamuts4, locals2, stackHeight2, nodesByLine2, Some(consumerCallableResultLine), consumerCallableDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts3, locals1, stackHeight1, nodesByLine1, consumerExpr2);

    val (nodesByLine6, destroyArraySequenceCallNode) =
      addLine(nodesByLine2,
        DestroyKnownSizeArrayH(
          newId(nodesByLine2),
          arrayExprResultLine.expectKnownSizeArrayAccess(),
          consumerCallableResultLine.expectInterfaceAccess()))

    val (hamuts8, locals4, stackHeight4, nodesByLine7) =
      ExpressionHammer.translateDeferreds(
        hinputs, hamuts4, locals2, stackHeight2, nodesByLine6, consumerCallableDeferreds ++ arrayExprDeferreds)

    (hamuts8, locals4, stackHeight4, nodesByLine7)
  }

  def translateDestroyUnknownSizeArray(
    hinputs: Hinputs,
    hamuts0: Hamuts,
    locals0: Locals,
    stackHeight0: StackHeight,
    nodesByLine0: Vector[NodeH],
    das2: DestroyUnknownSizeArray2):
  (Hamuts, Locals, StackHeight, Vector[NodeH]) = {
    val DestroyUnknownSizeArray2(arrayExpr2, unknownSizeArrayType2, consumerExpr2) = das2;

    val UnknownSizeArrayT2(RawArrayT2(memberType2, mutability)) = unknownSizeArrayType2

    val (hamuts1, arrayTypeH) =
      TypeHammer.translateUnknownSizeArray(hinputs, hamuts0, unknownSizeArrayType2)
    val (hamuts2, arrayRefTypeH) =
      TypeHammer.translateReference(hinputs, hamuts1, arrayExpr2.resultRegister.reference)
    vassert(arrayRefTypeH.expectUnknownSizeArrayReference().kind == arrayTypeH)

    val (hamuts3, locals1, stackHeight1, nodesByLine1, Some(arrayExprResultLine), arrayExprDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts2, locals0, stackHeight0, nodesByLine0, arrayExpr2);

    val (hamuts4, locals2, stackHeight2, nodesByLine2, Some(consumerCallableResultLine), consumerCallableDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts3, locals1, stackHeight1, nodesByLine1, consumerExpr2);

    val (nodesByLine6, destroyArraySequenceCallNode) =
      addLine(nodesByLine2,
        DestroyUnknownSizeArrayH(
          newId(nodesByLine2),
          arrayExprResultLine.expectUnknownSizeArrayAccess(),
          consumerCallableResultLine.expectInterfaceAccess()))

    val (hamuts8, locals4, stackHeight4, nodesByLine7) =
      ExpressionHammer.translateDeferreds(
        hinputs, hamuts4, locals2, stackHeight2, nodesByLine6, consumerCallableDeferreds ++ arrayExprDeferreds)

    (hamuts8, locals4, stackHeight4, nodesByLine7)
  }

  def translateIf(
      hinputs: Hinputs, hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      if2: If2):
  (Hamuts, Locals, StackHeight, Vector[NodeH], Option[RegisterAccessH[ReferendH]]) = {

    val If2(condition2, thenBlock2, elseBlock2) = if2

    val (hamuts1, locals1, conditionBlockH, maybeConditionResultAccess) =
      BlockHammer.translateBlock(hinputs, hamuts0, locals0, stackHeight0.oneBlockHigher(), condition2);
    vassert(maybeConditionResultAccess.get.expectedType == ReferenceH(m.Share, BoolH()))

    val (hamuts2, locals2, thenBlockH, maybeThenResultAccess) =
      BlockHammer.translateBlock(hinputs, hamuts1, locals1, stackHeight0.oneBlockHigher(), thenBlock2);
    val maybeThenResultCoord = maybeThenResultAccess.map(_.expectedType)

    val (hamuts3, locals3, elseBlockH, maybeElseResultAccess) =
      BlockHammer.translateBlock(hinputs, hamuts2, locals2, stackHeight0.oneBlockHigher(), elseBlock2);
    val maybeElseResultCoord = maybeElseResultAccess.map(_.expectedType)

    val (nodesByLine10, ifCallNode) =
      addLine(nodesByLine0,
        IfH(
          newId(nodesByLine0),
          conditionBlockH,
          thenBlockH,
          elseBlockH))

    val maybeResultCoord =
      (maybeThenResultCoord, maybeElseResultCoord) match {
        case (None, None) => None
        case (Some(ReferenceH(m.Raw, NeverH())), Some(ReferenceH(m.Raw, NeverH()))) => Some(ReferenceH(m.Raw, NeverH()))
        case (Some(ReferenceH(m.Raw, NeverH())), None) => None
        case (Some(ReferenceH(m.Raw, NeverH())), Some(elseResultCoord)) => Some(elseResultCoord)
        case (None, Some(ReferenceH(m.Raw, NeverH()))) => None
        case (Some(thenResultCoord), Some(ReferenceH(m.Raw, NeverH()))) => Some(thenResultCoord)
        case (Some(thenResultCoord), Some(elseResultCoord)) => {
          vassert(thenResultCoord == elseResultCoord, "what\n" + maybeThenResultCoord + "\n" + maybeElseResultCoord)
          // Arbitrarily choose the then
          Some(thenResultCoord)
        }
      }
    val maybeResultAccess = maybeResultCoord.map(coord => RegisterAccessH(ifCallNode.registerId, coord))

    (hamuts3, locals3, stackHeight0, nodesByLine10, maybeResultAccess)
  }

  def translateWhile(
      hinputs: Hinputs, hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      while2: While2):
  (Hamuts, Locals, StackHeight, Vector[NodeH]) = {

    val While2(bodyExpr2) = while2

    val (hamuts1, locals1, bodyBlockH, maybeBodyResultAccess) =
      BlockHammer.translateBlock(
        hinputs, hamuts0, locals0, stackHeight0.oneBlockHigher(), bodyExpr2);

    val (nodesByLine10, whileCallNode) =
      addLine(nodesByLine0,
        WhileH(
          newId(nodesByLine0),
          bodyBlockH))

    (hamuts1, locals1, stackHeight0, nodesByLine10)
  }

  def translateInterfaceFunctionCall(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      superFunctionHeader: FunctionHeader2,
      resultType2: Coord,
      argsExprs2: List[Expression2]):
  (Hamuts, Locals, StackHeight, Vector[NodeH], Option[RegisterAccessH[ReferendH]]) = {
    val (hamuts1, locals1, stackHeight1, nodesByLine1, argLines, argsDeferreds) =
      ExpressionHammer.translateExpressions(
        hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, argsExprs2);

    val virtualParamIndex = superFunctionHeader.getVirtualIndex.get
    val Coord(_, interfaceRef2 @ InterfaceRef2(_)) =
      superFunctionHeader.paramTypes(virtualParamIndex)
    val (hamuts4, interfaceRefH) =
      StructHammer.translateInterfaceRef(hinputs, hamuts1, interfaceRef2)
    val edge = hinputs.edgeBlueprintsByInterface(interfaceRef2)
    vassert(edge.interface == interfaceRef2)
    val indexInEdge = edge.superFamilyRootBanners.indexOf(superFunctionHeader.toBanner)
    vassert(indexInEdge >= 0)

    val (hamuts5, prototypeH) = FunctionHammer.translatePrototype(hinputs, hamuts4, superFunctionHeader.toPrototype)

    val (nodesByLine2, callNode) =
      addLine(
        nodesByLine1,
        InterfaceCallH(
          newId(nodesByLine1),
          argLines,
          virtualParamIndex,
          interfaceRefH,
          indexInEdge,
          prototypeH));
    val access =
      if (prototypeH.returnType == ReferenceH(m.Raw, VoidH())) {
        None
      } else {
        Some(RegisterAccessH(callNode.registerId, prototypeH.returnType))
      }

    val (hamuts6, locals2, stackHeight2, nodesByLine4) =
      ExpressionHammer.translateDeferreds(
        hinputs, hamuts5, locals1, stackHeight1, nodesByLine2, argsDeferreds)

    (hamuts6, locals2, stackHeight2, nodesByLine4, access)
    //
//    val (hamuts6, nodesByLine4, callResultLine) =
//      superFamilyRootBanner.params.zipWithIndex.collectFirst({
//        case (Parameter2(_, Some(_), Coord(_, interfaceRef2 : InterfaceRef2)), paramIndex) => {
//          val (hamuts3, interfaceRefH) =
//            StructHammer.translateInterfaceRef(hinputs, hamuts2, interfaceRef2)
//
//          val (hamuts6, nodesByLine2, functionNodeLine) =
//            translateInterfaceFunctionCallWithInterface(
//              hinputs,
//              hamuts3,
//              nodesByLine1,
//              superFamilyRootBanner,
//              paramIndex,
//              interfaceRefH,
//              functionTypeH,
//              argLines)
//          (hamuts6, nodesByLine2, functionNodeLine)
//        }
//        case (Parameter2(_, Some(_), Coord(_, structRef2@ StructRef2(_))), _) => {
//          val (hamuts2, nodesByLine1, functionRegister) =
//            translateInterfaceFunctionLookupWithStruct(
//              hinputs,
//              hamuts1,
//              nodesByLine0,
//              structRef2,
//              superFamilyRootBanner)
//          val (nodesByLine3, callResultNode) =
//            addLine(
//              nodesByLine1,
//              CallH(
//                newId(nodesByLine1),
//                functionRegister,
//                argLines));
//
//          val returnType2 = functionRegister.expectedType.expectFunctionReference().innerType.returnType
//          val access =
//            if (returnType2 == ReferenceH(m.Raw, VoidH())) {
//              None
//            } else {
//              Some(RegisterAccessH(callResultNode.registerId, returnType2))
//            }
//          (hamuts2, nodesByLine3, access)
//        }
//      }).get
//
//    val (hamuts5, locals2, stackHeight2, nodesByLine5) =
//      ExpressionHammer.translateDeferreds(
//        hinputs, hamuts6, locals1, stackHeight1, nodesByLine4, argsDeferreds)
//    (hamuts5, locals2, stackHeight2, nodesByLine5, callResultLine)
  }
//
//  private def translateInterfaceFunctionLookupWithStruct(
//      hinputs: Hinputs,
//      hamuts0: Hamuts,
//      nodesByLine0: Vector[NodeH],
//      structRef2: StructRef2,
//      superFamilyRootBanner: FunctionBanner2):
//  (Hamuts, Vector[NodeH], RegisterAccessH[FunctionTH]) = {
//    val prototype2 =
//      getPrototypeForStructInterfaceCall(hinputs, structRef2, superFamilyRootBanner)
//
//    val (hamuts1, functionRefH) =
//      FunctionHammer.translateFunctionRef(hinputs, hamuts0, prototype2);
//    val (nodesByLine1, functionNode) =
//      addLine(
//        nodesByLine0,
//        LoadFunctionH(newId(nodesByLine0), functionRefH));
//    val access = RegisterAccessH(functionNode.registerId, ReferenceH(m.Raw, functionRefH.functionType))
//    (hamuts1, nodesByLine1, access)
//  }
//
//  private def translateInterfaceFunctionCallWithInterface(
//      hinputs: Hinputs,
//      hamuts0: Hamuts,
//      nodesByLine0: Vector[NodeH],
//      superFamilyRootBanner: FunctionBanner2,
//      firstVirtualParamIndex: Int,
//      firstVirtualParamInterface: InterfaceRefH,
//      functionTypeH: FunctionTH,
//      argLines: List[RegisterAccessH[ReferendH]]):
//  (Hamuts, Vector[NodeH], Option[RegisterAccessH[ReferendH]]) = {
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
//    val (nodesByLine2, methodNode) =
//      addLine(
//        nodesByLine0,
//        InterfaceCallH(
//          newId(nodesByLine0),
//          argLines,
//          firstVirtualParamIndex,
//          firstVirtualParamInterface,
//          interfaceId,
//          indexInEdge,
//          functionTypeH));
//
//    val access =
//      if (functionTypeH.returnType == ReferenceH(m.Raw, VoidH())) {
//        None
//      } else {
//        Some(RegisterAccessH(methodNode.registerId, functionTypeH.returnType))
//      }
//    (hamuts0, nodesByLine2, access)
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
