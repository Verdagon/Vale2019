package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.templata.{FunctionBanner2, Prototype2}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{vassert, vfail}

object CallHammer {

  def newId(nodesByLine: Vector[Node3]) = Hammer.newId(nodesByLine)

  def addLine(nodesByLine: Vector[Node3], node: Node3): (Vector[Node3], Node3) = {
    (nodesByLine :+ node, node)
  }

  def translateExternFunctionCall(
    hinputs: Hinputs,
    hamuts0: Hamuts,
    locals0: Locals,
    stackHeight0: StackHeight,
    nodesByLine0: Vector[Node3],
    prototype2: Prototype2,
    argsExprs2: List[ReferenceExpression2]):
  (Hamuts, Locals, StackHeight, Vector[Node3], Option[RegisterAccess3[Referend3]]) = {
    val (hamuts1, locals1, stackHeight1, nodesByLine1, argsResultLines, argsDeferreds) =
      ExpressionHammer.translateExpressions(
        hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, argsExprs2);

    // Doublecheck the types
    val (hamuts2, paramTypes) =
      TypeHammer.translateReferences(hinputs, hamuts1, prototype2.functionType.paramTypes);
    vassert(argsResultLines.map(_.expectedType) == paramTypes)

    val (hamuts3, functionRef3) =
      FunctionHammer.translateFunctionRef(hinputs, hamuts2, prototype2);

    val (nodesByLine3, callResultNode) =
      addLine(
        nodesByLine1,
        ExternCall3(
          newId(nodesByLine1),
          functionRef3,
          argsResultLines))

    val (hamuts5, locals2, stackHeight2, nodesByLine4) =
      ExpressionHammer.translateDeferreds(
        hinputs, hamuts3, locals1, stackHeight1, nodesByLine3, argsDeferreds)

    val access =
      if (prototype2.functionType.returnType == Coord(Raw, Void2())) {
        None
      } else {
        Some(RegisterAccess3(callResultNode.registerId, functionRef3.functionType.returnType))
      }
    (hamuts5, locals2, stackHeight2, nodesByLine4, access)
  }

  def translateFunctionPointerCall(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      callableExpr: ReferenceExpression2,
      args: List[Expression2],
      resultType2: Coord):
  (Hamuts, Locals, StackHeight, Vector[Node3], Option[RegisterAccess3[Referend3]]) = {
    val Coord(Raw, FunctionT2(paramTypes, returnType2)) = callableExpr.resultRegister.reference
    val (hamuts2, locals1, stackHeight1, nodesByLine1, argLines, argsDeferreds) =
      ExpressionHammer.translateExpressions(
        hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, args);

    val (hamuts3, locals2, stackHeight2, nodesByLine2, Some(functionLine), callableDeferreds) =
      ExpressionHammer.translate(hinputs, hamuts2, locals1, stackHeight1,  nodesByLine1, callableExpr)

    // Doublecheck the types
    val (hamuts4, paramTypes3) =
      TypeHammer.translateReferences(hinputs, hamuts3, paramTypes)
    vassert(argLines.map(_.expectedType) == paramTypes3)

    // Doublecheck return
    val (hamuts5, returnType3) = TypeHammer.translateReference(hinputs, hamuts4, returnType2)
    val (hamuts6, resultType3) = TypeHammer.translateReference(hinputs, hamuts5, resultType2);
    vassert(returnType3 == resultType3)

    val (nodesByLine3, callResultNode) =
      addLine(
        nodesByLine2,
        Call3(
          newId(nodesByLine2),
          functionLine.expectFunctionAccess(),
          argLines));

    val (hamuts7, locals3, stackHeight3, nodesByLine4) =
      ExpressionHammer.translateDeferreds(
        hinputs, hamuts6, locals2, stackHeight2, nodesByLine3, argsDeferreds ++ callableDeferreds)

    val access =
      if (returnType2 == Coord(Raw, Void2())) {
        None
      } else {
        Some(RegisterAccess3(callResultNode.registerId, resultType3))
      }
    (hamuts7, locals3, stackHeight3, nodesByLine4, access)
  }

  def translateConstructArray(
      hinputs: Hinputs, hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      constructArray2: ConstructArray2):
  (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[UnknownSizeArrayT3]) = {
    val ConstructArray2(arrayType2, sizeExpr2, callExpr2) = constructArray2;

    val (hamuts1, locals1, stackHeight1, nodesByLine1, Some(sizeRegisterId), sizeDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, sizeExpr2);

    val FunctionPointerCall2(callableExpr2, argsExprs2IncludingPlaceholder) = callExpr2
    val (hamuts2, locals2, stackHeight2, nodesByLine2, Some(generatorCallableResultLine), generatorCallableDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts1, locals1, stackHeight1, nodesByLine1, callableExpr2);

    val argsExprs2NotIncludingPlaceholder = argsExprs2IncludingPlaceholder.init
    vassert(argsExprs2IncludingPlaceholder.last == Placeholder2(Coord(Share, Int2())))

    val (hamuts3, generatorFunctionType3) =
      TypeHammer.translateFunction(hinputs, hamuts2, callExpr2.functionType)
    vassert(generatorCallableResultLine.expectedType.kind == generatorFunctionType3)

    val (hamuts4, locals3, stackHeight3, nodesByLine5, generatorArgsLinesNotIncludingPlaceholder, generatorArgsDeferreds) =
      ExpressionHammer.translateExpressions(
        hinputs, hamuts3, locals2, stackHeight2, nodesByLine2, argsExprs2NotIncludingPlaceholder);

    val (nodesByLine6, indexPlaceholderNode) =
      addLine(nodesByLine5, Placeholder3(newId(nodesByLine5), Reference3(Share, Int3())))
    val indexPlaceholderAccess = RegisterAccess3(indexPlaceholderNode.registerId, Reference3(Share, Int3()))

    val generatorArgsLinesIncludingPlaceholder =
      generatorArgsLinesNotIncludingPlaceholder :+ indexPlaceholderAccess

    val (hamuts5, generatorParamTypes3) =
      TypeHammer.translateReferences(
        hinputs, hamuts4, callExpr2.args.map(_.resultRegister.reference))
    if (generatorArgsLinesIncludingPlaceholder.map(_.expectedType) != generatorParamTypes3) {
      vfail("wat")
    }

    val (hamuts6, arrayRefType3) =
      TypeHammer.translateReference(
        hinputs, hamuts5, constructArray2.resultRegister.reference)

    val (hamuts7, arrayType3) =
      TypeHammer.translateUnknownSizeArray(hinputs, hamuts6, arrayType2)
    vassert(arrayRefType3.expectUnknownSizeArrayReference().kind == arrayType3)

    val (nodesByLine10, constructArrayCallNode) =
      addLine(nodesByLine6,
        ConstructArrayCall3(
          newId(nodesByLine6),
          sizeRegisterId.expectIntAccess(),
          generatorCallableResultLine.expectFunctionAccess(),
          generatorArgsLinesIncludingPlaceholder,
          arrayRefType3.expectUnknownSizeArrayReference()))

    val (hamuts8, locals4, stackHeight4, nodesByLine11) =
      ExpressionHammer.translateDeferreds(
        hinputs, hamuts7, locals3, stackHeight3, nodesByLine10, generatorArgsDeferreds ++ generatorCallableDeferreds ++ sizeDeferreds)

    val access =
      RegisterAccess3(
        constructArrayCallNode.registerId,
        arrayRefType3.expectUnknownSizeArrayReference())
    (hamuts8, locals4, stackHeight4, nodesByLine11, access)
  }

  def translateDestroyArraySequence(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      das2: DestroyArraySequence2):
  (Hamuts, Locals, StackHeight, Vector[Node3]) = {
    val DestroyArraySequence2(arrayExpr2, arraySequenceType, callExpr2) = das2;

    val ArraySequenceT2(size, rawArrayType2 @ RawArrayT2(memberType2, mutability)) = arraySequenceType

    val (hamuts1, arrayType3) =
      TypeHammer.translateKnownSizeArray(hinputs, hamuts0, arraySequenceType)
    val (hamuts2, arrayRefType3) =
      TypeHammer.translateReference(hinputs, hamuts1, arrayExpr2.resultRegister.reference)
    vassert(arrayRefType3.expectKnownSizeArrayReference().kind == arrayType3)

    val (hamuts3, locals1, stackHeight1, nodesByLine1, Some(arrayExprResultLine), arrayExprDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts2, locals0, stackHeight0, nodesByLine0, arrayExpr2);

    val FunctionPointerCall2(callableExpr2, argsExprs2IncludingPlaceholders) = callExpr2
    val (hamuts4, locals2, stackHeight2, nodesByLine2, Some(consumerCallableResultLine), consumerCallableDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts3, locals1, stackHeight1, nodesByLine1, callableExpr2);

    val (hamuts5, consumerFunctionType3) =
      TypeHammer.translateFunction(hinputs, hamuts4, callExpr2.functionType)
    vassert(consumerCallableResultLine.expectedType.kind == consumerFunctionType3)

    val placeholderArgsExprs2 = argsExprs2IncludingPlaceholders.last
    if (List(placeholderArgsExprs2) != List(Placeholder2(memberType2))) {
      vfail("wat")
    }

    val argsExprs2NotIncludingPlaceholders = argsExprs2IncludingPlaceholders.init

    val (hamuts6, locals3, stackHeight3, nodesByLine3, argsLines3NotIncludingPlaceholders, consumerArgsDeferreds) =
      ExpressionHammer.translateExpressions(
        hinputs, hamuts5, locals2, stackHeight2, nodesByLine2, argsExprs2NotIncludingPlaceholders);

    val (nodesByLine4, elementPlaceholderNode) =
      addLine(nodesByLine3, Placeholder3(newId(nodesByLine3), arrayType3.rawArray.elementType))
    val elementPlaceholderAccess = RegisterAccess3(elementPlaceholderNode.registerId, arrayType3.rawArray.elementType)

//    val (nodesByLine5, indexPlaceholderNode) =
//      addLine(nodesByLine4, Placeholder3(newId(nodesByLine4), Reference3(Share, Int3())))
//    val indexPlaceholderAccess = RegisterAccess3(indexPlaceholderNode.registerId, Reference3(Share, Int3()))

    val consumerArgsLines =
      argsLines3NotIncludingPlaceholders :+ elementPlaceholderAccess

    val (hamuts7, consumerParamTypes3) =
      TypeHammer.translateReferences(
        hinputs, hamuts6, callExpr2.args.map(_.resultRegister.reference))
    if (consumerArgsLines.map(_.expectedType) != consumerParamTypes3) {
      vfail("wat")
    }

    val (nodesByLine6, destroyArraySequenceCallNode) =
      addLine(nodesByLine4,
        DestroyKnownSizeArray3(
          newId(nodesByLine4),
          arrayExprResultLine.expectKnownSizeArrayAccess(),
          consumerCallableResultLine.expectFunctionAccess(),
          consumerArgsLines))

    val (hamuts8, locals4, stackHeight4, nodesByLine7) =
      ExpressionHammer.translateDeferreds(
        hinputs, hamuts7, locals3, stackHeight3, nodesByLine6, consumerArgsDeferreds ++ consumerCallableDeferreds)

    (hamuts8, locals4, stackHeight4, nodesByLine7)
  }

  def translateDestroyUnknownSizeArray(
    hinputs: Hinputs,
    hamuts0: Hamuts,
    locals0: Locals,
    stackHeight0: StackHeight,
    nodesByLine0: Vector[Node3],
    das2: DestroyUnknownSizeArray2):
  (Hamuts, Locals, StackHeight, Vector[Node3]) = {
    val DestroyUnknownSizeArray2(arrayExpr2, unknownSizeArrayType2, callExpr2) = das2;

    val UnknownSizeArrayT2(RawArrayT2(memberType2, mutability)) = unknownSizeArrayType2

    val (hamuts1, arrayType3) =
      TypeHammer.translateUnknownSizeArray(hinputs, hamuts0, unknownSizeArrayType2)
    val (hamuts2, arrayRefType3) =
      TypeHammer.translateReference(hinputs, hamuts1, arrayExpr2.resultRegister.reference)
    vassert(arrayRefType3.expectUnknownSizeArrayReference().kind == arrayType3)

    val (hamuts3, locals1, stackHeight1, nodesByLine1, Some(arrayExprResultLine), arrayExprDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts2, locals0, stackHeight0, nodesByLine0, arrayExpr2);

    val FunctionPointerCall2(callableExpr2, argsExprs2IncludingPlaceholders) = callExpr2
    val (hamuts4, locals2, stackHeight2, nodesByLine2, Some(consumerCallableResultLine), consumerCallableDeferreds) =
      ExpressionHammer.translate(
        hinputs, hamuts3, locals1, stackHeight1, nodesByLine1, callableExpr2);

    val (hamuts5, consumerFunctionType3) =
      TypeHammer.translateFunction(hinputs, hamuts4, callExpr2.functionType)
    vassert(consumerCallableResultLine.expectedType.kind == consumerFunctionType3)

    val placeholderArgsExprs2 = argsExprs2IncludingPlaceholders.last
    if (List(placeholderArgsExprs2) != List(Placeholder2(memberType2))) {
      vfail("wat")
    }

    val argsExprs2NotIncludingPlaceholders = argsExprs2IncludingPlaceholders.init

    val (hamuts6, locals3, stackHeight3, nodesByLine3, argsLines3NotIncludingPlaceholders, consumerArgsDeferreds) =
      ExpressionHammer.translateExpressions(
        hinputs, hamuts5, locals2, stackHeight2, nodesByLine2, argsExprs2NotIncludingPlaceholders);

    val (nodesByLine4, elementPlaceholderNode) =
      addLine(nodesByLine3, Placeholder3(newId(nodesByLine3), arrayType3.rawArray.elementType))
    val elementPlaceholderAccess = RegisterAccess3(elementPlaceholderNode.registerId, arrayType3.rawArray.elementType)

    //    val (nodesByLine5, indexPlaceholderNode) =
    //      addLine(nodesByLine4, Placeholder3(newId(nodesByLine4), Reference3(Share, Int3())))
    //    val indexPlaceholderAccess = RegisterAccess3(indexPlaceholderNode.registerId, Reference3(Share, Int3()))

    val consumerArgsLines =
      argsLines3NotIncludingPlaceholders :+ elementPlaceholderAccess

    val (hamuts7, consumerParamTypes3) =
      TypeHammer.translateReferences(
        hinputs, hamuts6, callExpr2.args.map(_.resultRegister.reference))
    if (consumerArgsLines.map(_.expectedType) != consumerParamTypes3) {
      vfail("wat")
    }

    val (nodesByLine6, destroyArraySequenceCallNode) =
      addLine(nodesByLine4,
        DestroyUnknownSizeArray3(
          newId(nodesByLine4),
          arrayExprResultLine.expectUnknownSizeArrayAccess(),
          consumerCallableResultLine.expectFunctionAccess(),
          consumerArgsLines))

    val (hamuts8, locals4, stackHeight4, nodesByLine7) =
      ExpressionHammer.translateDeferreds(
        hinputs, hamuts7, locals3, stackHeight3, nodesByLine6, consumerArgsDeferreds ++ consumerCallableDeferreds)

    (hamuts8, locals4, stackHeight4, nodesByLine7)
  }

  def translateIf(
      hinputs: Hinputs, hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      if2: If2):
  (Hamuts, Locals, StackHeight, Vector[Node3], Option[RegisterAccess3[Referend3]]) = {

    val If2(condition2, thenBlock2, elseBlock2) = if2

    val (hamuts1, locals1, conditionBlock3, maybeConditionResultAccess) =
      BlockHammer.translateBlock(hinputs, hamuts0, locals0, stackHeight0.oneBlockHigher(), condition2);
    vassert(maybeConditionResultAccess.get.expectedType == Reference3(Share, Bool3()))

    val (hamuts2, locals2, thenBlock3, maybeThenResultAccess) =
      BlockHammer.translateBlock(hinputs, hamuts1, locals1, stackHeight0.oneBlockHigher(), thenBlock2);
    val maybeThenResultCoord = maybeThenResultAccess.map(_.expectedType)

    val (hamuts3, locals3, elseBlock3, maybeElseResultAccess) =
      BlockHammer.translateBlock(hinputs, hamuts2, locals2, stackHeight0.oneBlockHigher(), elseBlock2);
    val maybeElseResultCoord = maybeElseResultAccess.map(_.expectedType)

    val (nodesByLine10, ifCallNode) =
      addLine(nodesByLine0,
        If3(
          newId(nodesByLine0),
          conditionBlock3,
          thenBlock3,
          elseBlock3))

    val maybeResultCoord =
      (maybeThenResultCoord, maybeElseResultCoord) match {
        case (None, None) => None
        case (Some(Reference3(Raw, Never3())), Some(Reference3(Raw, Never3()))) => Some(Reference3(Raw, Never3()))
        case (Some(Reference3(Raw, Never3())), None) => None
        case (Some(Reference3(Raw, Never3())), Some(elseResultCoord)) => Some(elseResultCoord)
        case (None, Some(Reference3(Raw, Never3()))) => None
        case (Some(thenResultCoord), Some(Reference3(Raw, Never3()))) => Some(thenResultCoord)
        case (Some(thenResultCoord), Some(elseResultCoord)) => {
          vassert(thenResultCoord == elseResultCoord, "what\n" + maybeThenResultCoord + "\n" + maybeElseResultCoord)
          // Arbitrarily choose the then
          Some(thenResultCoord)
        }
      }
    val maybeResultAccess = maybeResultCoord.map(coord => RegisterAccess3(ifCallNode.registerId, coord))

    (hamuts3, locals3, stackHeight0, nodesByLine10, maybeResultAccess)
  }

  def translateWhile(
      hinputs: Hinputs, hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      while2: While2):
  (Hamuts, Locals, StackHeight, Vector[Node3]) = {

    val While2(bodyExpr2) = while2

    val (hamuts1, locals1, bodyBlock3, maybeBodyResultAccess) =
      BlockHammer.translateBlock(
        hinputs, hamuts0, locals0, stackHeight0.oneBlockHigher(), bodyExpr2);

    val (nodesByLine10, whileCallNode) =
      addLine(nodesByLine0,
        While3(
          newId(nodesByLine0),
          bodyBlock3))

    (hamuts1, locals1, stackHeight0, nodesByLine10)
  }

  def translateInterfaceFunctionCall(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      superFunctionBanner: FunctionBanner2,
      functionTypeReference2: Coord,
      resultType2: Coord,
      argsExprs2: List[Expression2]):
  (Hamuts, Locals, StackHeight, Vector[Node3], Option[RegisterAccess3[Referend3]]) = {
//    // The function family that this interface call was originally for may have gotten
//    // merged into a different family (or multiple).
//    // We are guaranteed that the family it merged into can handle all uses of this though.
//    val superFamilyRootBanner =
//      hinputs.superFamilyRootBannersBySignature.get(subFamilyRootBanner.toSignature) match {
//        case Some(x) => x
//        case None => {
//          vfail("noooo")
//        }
//      }
//    vassert(superFamilyRootBanner.params.size == argsExprs2.size);
//
    val (hamuts1, locals1, stackHeight1, nodesByLine1, argLines, argsDeferreds) =
      ExpressionHammer.translateExpressions(
        hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, argsExprs2);

    val functionType2 =
      functionTypeReference2 match {
        case Coord(Raw, ft2 @ FunctionT2(_, _)) => ft2
      }
    val (hamuts3, functionType3) =
      TypeHammer.translateFunction(hinputs, hamuts1, functionType2);

    val virtualParamIndex = superFunctionBanner.getVirtualIndex.get
    val Coord(_, interfaceRef2 @ InterfaceRef2(_)) =
      superFunctionBanner.paramTypes(virtualParamIndex)
    val (hamuts4, interfaceRef3) =
      StructHammer.translateInterfaceRef(hinputs, hamuts3, interfaceRef2)
    val edge = hinputs.edgeBlueprintsByInterface(interfaceRef2)
    vassert(edge.interface == interfaceRef2)
    val indexInEdge = edge.superFamilyRootBanners.indexOf(superFunctionBanner)
    vassert(indexInEdge >= 0)

    val (nodesByLine2, callNode) =
      addLine(
        nodesByLine1,
        InterfaceCall3(
          newId(nodesByLine1),
          argLines,
          virtualParamIndex,
          interfaceRef3,
          interfaceRef3.interfaceId,
          indexInEdge,
          functionType3));
    val access =
      if (functionType3.returnType == Reference3(Raw, Void3())) {
        None
      } else {
        Some(RegisterAccess3(callNode.registerId, functionType3.returnType))
      }
    (hamuts4, locals1, stackHeight1, nodesByLine2, access)
    //
//    val (hamuts6, nodesByLine4, callResultLine) =
//      superFamilyRootBanner.params.zipWithIndex.collectFirst({
//        case (Parameter2(_, Some(_), Coord(_, interfaceRef2 : InterfaceRef2)), paramIndex) => {
//          val (hamuts3, interfaceRef3) =
//            StructHammer.translateInterfaceRef(hinputs, hamuts2, interfaceRef2)
//
//          val (hamuts6, nodesByLine2, functionNodeLine) =
//            translateInterfaceFunctionCallWithInterface(
//              hinputs,
//              hamuts3,
//              nodesByLine1,
//              superFamilyRootBanner,
//              paramIndex,
//              interfaceRef3,
//              functionType3,
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
//              Call3(
//                newId(nodesByLine1),
//                functionRegister,
//                argLines));
//
//          val returnType2 = functionRegister.expectedType.expectFunctionReference().innerType.returnType
//          val access =
//            if (returnType2 == Reference3(Raw, Void3())) {
//              None
//            } else {
//              Some(RegisterAccess3(callResultNode.registerId, returnType2))
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

  private def translateInterfaceFunctionLookupWithStruct(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      nodesByLine0: Vector[Node3],
      structRef2: StructRef2,
      superFamilyRootBanner: FunctionBanner2):
  (Hamuts, Vector[Node3], RegisterAccess3[FunctionT3]) = {
    val prototype2 =
      getPrototypeForStructInterfaceCall(hinputs, structRef2, superFamilyRootBanner)

    val (hamuts1, functionRef3) =
      FunctionHammer.translateFunctionRef(hinputs, hamuts0, prototype2);
    val (nodesByLine1, functionNode) =
      addLine(
        nodesByLine0,
        LoadFunction3(newId(nodesByLine0), functionRef3));
    val access = RegisterAccess3(functionNode.registerId, Reference3(Raw, functionRef3.functionType))
    (hamuts1, nodesByLine1, access)
  }

  private def translateInterfaceFunctionCallWithInterface(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      nodesByLine0: Vector[Node3],
      superFamilyRootBanner: FunctionBanner2,
      firstVirtualParamIndex: Int,
      firstVirtualParamInterface: InterfaceRef3,
      functionType3: FunctionT3,
      argLines: List[RegisterAccess3[Referend3]]):
  (Hamuts, Vector[Node3], Option[RegisterAccess3[Referend3]]) = {
    val interfaceId = firstVirtualParamInterface.interfaceId

    val edgeBlueprint =
      hinputs.edgeBlueprintsByInterfaceId(interfaceId)
    val indexInEdge =
      edgeBlueprint.superFamilyRootBanners.indexOf(superFamilyRootBanner)
    if (indexInEdge < 0) {
      vfail("Can't find:\n" + superFamilyRootBanner + "\nin:\n" + edgeBlueprint.interface)
    }

    val (nodesByLine2, methodNode) =
      addLine(
        nodesByLine0,
        InterfaceCall3(
          newId(nodesByLine0),
          argLines,
          firstVirtualParamIndex,
          firstVirtualParamInterface,
          interfaceId,
          indexInEdge,
          functionType3));

    val access =
      if (functionType3.returnType == Reference3(Raw, Void3())) {
        None
      } else {
        Some(RegisterAccess3(methodNode.registerId, functionType3.returnType))
      }
    (hamuts0, nodesByLine2, access)
  }

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
