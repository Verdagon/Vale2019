package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.AddressibleLocalVariable2
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{vassert, vassertSome, vcurious, vfail}

object ExpressionHammer {

  def newId(nodesByLine: Vector[Node3]): String = Hammer.newId(nodesByLine)

  // stackHeight is the number of locals that have been declared in ancestor
  // blocks and previously in this block. It's used to figure out the index of
  // a newly declared local.
  // Returns:
  // - Hamuts, containing structs and stuff
  // - locals
  // - nodesByLine
  // - result register id
  // - deferred expressions, to move to after the enclosing call. head is put first after call.
  def translate(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      expr2: Expression2):
      (Hamuts, Locals, StackHeight, Vector[Node3], Option[RegisterAccess3[Referend3]], List[Expression2]) = {
    expr2 match {
      case IntLiteral2(value) => {
        val (nodesByLine1, resultNode) =
          addNode(nodesByLine0, ConstantI643(newId(nodesByLine0), value));
        val access = RegisterAccess3[Referend3](resultNode.registerId, Reference3(Share, Int3()))
        (hamuts0, locals0, stackHeight0, nodesByLine1, Some(access), List())
      }
      case VoidLiteral2() => {
        val (nodesByLine1, resultNode) =
          addNode(nodesByLine0, ConstantVoid3(newId(nodesByLine0)));
        (hamuts0, locals0, stackHeight0, nodesByLine1, None, List())
      }
      case StrLiteral2(value) => {
        val (nodesByLine1, resultNode) =
          addNode(nodesByLine0, ConstantStr3(newId(nodesByLine0), value));
        val access = RegisterAccess3[Referend3](resultNode.registerId, Reference3(Share, Str3()))
        (hamuts0, locals0, stackHeight0, nodesByLine1, Some(access), List())
      }
      case FloatLiteral2(value) => {
        val (nodesByLine1, resultNode) =
            addNode(nodesByLine0, ConstantF643(newId(nodesByLine0), value));
        val access = RegisterAccess3[Referend3](resultNode.registerId, Reference3(Share, Float3()))
        (hamuts0, locals0, stackHeight0, nodesByLine1, Some(access), List())
      }
      case BoolLiteral2(value) => {
        val (nodesByLine1, resultNode) =
            addNode(nodesByLine0, ConstantBool3(newId(nodesByLine0), value));
        val access = RegisterAccess3[Referend3](resultNode.registerId, Reference3(Share, Bool3()))
        (hamuts0, locals0, stackHeight0, nodesByLine1, Some(access), List())
      }
      case let2 @ LetNormal2(_, _) => {
        val (hamuts5, locals3, stackHeight1, nodesByLine5) =
          LetHammer.translateLet(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, let2)
        (hamuts5, locals3, stackHeight1, nodesByLine5, None, List())
      }
      case let2 @ LetAndLend2(_, _) => {
        val (hamuts5, locals3, stackHeight1, nodesByLine5, borrowAccess) =
          LetHammer.translateLetAndLend(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, let2)
        (hamuts5, locals3, stackHeight1, nodesByLine5, Some(borrowAccess), List())
      }
      case des2 @ Destructure2(_, _, _) => {
        val (hamuts5, locals3, stackHeight1, nodesByLine5) =
          LetHammer.translateDestructure(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, des2)
        (hamuts5, locals3, stackHeight1, nodesByLine5, None, List())
      }
      case unlet2 @ Unlet2(_) => {
        val (hamuts5, locals3, stackHeight1, nodesByLine5, valueAccess) =
          LetHammer.translateUnlet(
            hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, unlet2)
        (hamuts5, locals3, stackHeight1, nodesByLine5, Some(valueAccess), List())
      }
      case mutate2 @ Mutate2(_, _) => {
        val (hamuts7, locals5, stackHeight1, nodesByLine7, newEmptyPackStructNodeLine) =
          MutateHammer.translateMutate(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, mutate2)
        (hamuts7, locals5, stackHeight1, nodesByLine7, Some(newEmptyPackStructNodeLine), List())
      }
      case b @ Block2(_) => {
        val (hamuts1, locals1, block3, maybeBlockResultAccess) =
          BlockHammer.translateBlock(hinputs, hamuts0, locals0, stackHeight0.oneBlockHigher(), b)
        val (nodesByLine1, inlineBlockResultNode) =
          addNode(nodesByLine0, InlineBlock3(newId(nodesByLine0), block3));
        val maybeAccess =
          maybeBlockResultAccess match {
            case None => None
            case Some(access) => {
              Some(RegisterAccess3[Referend3](inlineBlockResultNode.registerId, maybeBlockResultAccess.get.expectedType))
            }
          }
        (hamuts1, locals1, stackHeight0, nodesByLine1, maybeAccess, List())
      }
      case FunctionLookup2(prototype2) => {
        val (hamuts1, functionRef3) =
          FunctionHammer.translateFunctionRef(hinputs, hamuts0, prototype2);
        val (nodesByLine1, functionNode) =
            addNode(
              nodesByLine0,
              LoadFunction3(newId(nodesByLine0), functionRef3))
        val access = RegisterAccess3(functionNode.registerId, Reference3(Raw, functionRef3.functionType))
        (hamuts1, locals0, stackHeight0, nodesByLine1, Some(access), List())
      }
      case funcPtrCall2 @ FunctionPointerCall2(callableExpr, args) => {
        val (hamuts7, locals2, stackHeight1, nodesByLine4, access) =
          CallHammer.translateFunctionPointerCall(
            hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, callableExpr, args, funcPtrCall2.resultRegister.reference)
        (hamuts7, locals2, stackHeight1, nodesByLine4, access, List())
      }

      case InterfaceFunctionCall2(superFunctionBanner, functionType2, resultType2, argsExprs2) => {
        val (hamuts7, locals2, stackHeight1, nodesByLine4, access) =
          CallHammer.translateInterfaceFunctionCall(
            hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, superFunctionBanner, functionType2, resultType2, argsExprs2)
        (hamuts7, locals2, stackHeight1, nodesByLine4, access, List())
      }

      case Consecutor2(exprs) => {
        val (hamuts1, locals1, stackHeight1, nodesByLine1, resultLines, deferreds) =
          translateMaybeReturningExpressions(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, exprs);
        vassert(deferreds.isEmpty) // curiosity, would we have any here?
        (hamuts1, locals1, stackHeight1, nodesByLine1, resultLines.last, List())
      }

      case PackE2(exprs, resultType, resultPackType) => {
        val (hamuts1, locals1, stackHeight1, nodesByLine7, resultLines, deferreds) =
          translateExpressions(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, exprs);
        val (hamuts2, underlyingStructRef3) =
          StructHammer.translateStructRef(hinputs, hamuts1, resultPackType.underlyingStruct);
        val (hamuts3, resultReference) =
          TypeHammer.translateReference(hinputs, hamuts2, resultType)
        vassert(resultReference.kind == underlyingStructRef3)

        val newStructNode =
          NewStruct3(
            newId(nodesByLine7),
            resultLines,
            resultReference.expectStructReference())

        val (nodesByLine8, _) = addNode(nodesByLine7, newStructNode);

        val (hamuts4, locals9, stackHeight2, nodesByLine9) =
          translateDeferreds(hinputs, hamuts3, locals1, stackHeight1, nodesByLine8, deferreds)

        val access = RegisterAccess3(newStructNode.registerId, resultReference)
        // Export locals from inside the pack
        (hamuts4, locals9, stackHeight2, nodesByLine9, Some(access), List())
      }

      case ArrayLength2(arrayExpr2) => {
        val (hamuts1, locals1, stackHeight1, nodesByLine1, resultLine, deferreds) =
          translate(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, arrayExpr2);

        val (nodesByLine2, lengthResultNode) =
          addNode(nodesByLine1, ArrayLength3(newId(nodesByLine1), vassertSome(resultLine)));

        val (hamuts3, locals3, stackHeight3, nodesByLine3) =
          translateDeferreds(hinputs, hamuts1, locals1, stackHeight1, nodesByLine2, deferreds)

        val access = RegisterAccess3[Referend3](lengthResultNode.registerId, Reference3(Share, Int3()))
        (hamuts3, locals3, stackHeight3, nodesByLine3, Some(access), List())
      }

      case TupleE2(exprs, resultType, resultPackType) => {
        val (hamuts1, locals1, stackHeight1, nodesByLine7, resultLines, deferreds) =
          translateExpressions(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, exprs);
        val (hamuts2, underlyingStructRef3) =
          StructHammer.translateStructRef(hinputs, hamuts1, resultPackType.underlyingStruct);
        val (hamuts3, resultReference) =
          TypeHammer.translateReference(hinputs, hamuts2, resultType)
        vassert(resultReference.kind == underlyingStructRef3)

        val newStructNode =
          NewStruct3(
            newId(nodesByLine7),
            resultLines,
            resultReference.expectStructReference())
        val (nodesByLine8, _) = addNode(nodesByLine7, newStructNode);
        // Export locals from inside the pack

        val (hamuts4, locals2, stackHeight2, nodesByLine9) =
          translateDeferreds(hinputs, hamuts3, locals1, stackHeight1, nodesByLine8, deferreds)

        val access = RegisterAccess3(newStructNode.registerId, resultReference)
        (hamuts4, locals2, stackHeight2, nodesByLine9, Some(access), List())
      }

      case ArraySequenceE2(exprs, arrayReference2, arrayType2) => {
        val (hamuts1, locals1, stackHeight1, nodesByLine7, resultLines, deferreds) =
          translateExpressions(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, exprs);
        val (hamuts2, underlyingArray3) =
          TypeHammer.translateKnownSizeArray(hinputs, hamuts1, arrayType2);

        val (hamuts3, arrayReference3) =
          TypeHammer.translateReference(hinputs, hamuts2, arrayReference2)
        vassert(arrayReference3.kind == underlyingArray3)

        val newStructNode =
          NewArrayFromValues3(
            newId(nodesByLine7),
            resultLines,
            arrayReference3.expectKnownSizeArrayReference())
        val (nodesByLine8, _) = addNode(nodesByLine7, newStructNode);

        val (hamuts4, locals2, stackHeight2, nodesByLine9) =
          translateDeferreds(hinputs, hamuts3, locals1, stackHeight1, nodesByLine8, deferreds)

        val access = RegisterAccess3(newStructNode.registerId, arrayReference3)
        (hamuts4, locals2, stackHeight2, nodesByLine9, Some(access), List())
      }

      case Construct2(_, resultType2, memberExprs) => {
        val (hamuts1, locals1, stackHeight1, nodesByLine1, memberResultLines, deferreds) =
          translateExpressions(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, memberExprs);

        val (hamuts2, resultType3) =
          TypeHammer.translateReference(hinputs, hamuts1, resultType2)

//        hinputs.program2.lookupStruct(resultStructType2)
//        vassert(structDef2.getRef == resultType3.innerType)

        val newStructNode =
          NewStruct3(
            newId(nodesByLine1),
            memberResultLines,
            resultType3.expectStructReference())

        val (nodesByLine2, _) = addNode(nodesByLine1, newStructNode);

        val (hamuts3, locals2, stackHeight2, nodesByLine3) =
          translateDeferreds(hinputs, hamuts2, locals1, stackHeight1, nodesByLine2, deferreds)

        val access = RegisterAccess3(newStructNode.registerId, resultType3)
        (hamuts3, locals2, stackHeight2, nodesByLine3, Some(access), List())
      }

      case load2 @ SoftLoad2(_, _) => {
        val (hamuts6, locals5, stackHeight1, nodesByLine6, loadedAccess3, deferreds) =
          LoadHammer.translateLoad(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, load2)
        (hamuts6, locals5, stackHeight1, nodesByLine6, Some(loadedAccess3), deferreds)
      }

      case lookup2 @ LocalLookup2(AddressibleLocalVariable2(_, _, _), _) => {
        val (hamuts3, locals1, stackHeight1, nodesByLine2, loadBoxAccess) =
          LoadHammer.translateLocalAddress(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, lookup2)
        (hamuts3, locals1, stackHeight1, nodesByLine2, Some(loadBoxAccess), List())
      }

      case lookup2 @ AddressMemberLookup2(_, _, _, _) => {
        val (hamuts3, locals1, stackHeight1, nodesByLine2, loadBoxAccess, deferreds) =
          LoadHammer.translateMemberAddress(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, lookup2)
        (hamuts3, locals1, stackHeight1, nodesByLine2, Some(loadBoxAccess), deferreds)
      }

      case if2 @ If2(_, _, _) => {
        val (hamuts11, locals5, stackHeight1, nodesByLine11, access) =
          CallHammer.translateIf(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, if2)
        (hamuts11, locals5, stackHeight1, nodesByLine11, access, List())
      }

      case ca2 @ ConstructArray2(_, _, _) => {
        val (hamuts8, locals1, stackHeight1, nodesByLine11, access) =
          CallHammer.translateConstructArray(
            hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, ca2)
        (hamuts8, locals1, stackHeight1, nodesByLine11, Some(access), List())
      }

      case TemplarReinterpret2(innerExpr, resultType2) => {
        // Check types; it's overkill because reinterprets are rather scary.
        val innerExprResultType2 = innerExpr.resultRegister.reference
        val (hamuts1, innerExprResultType3) = TypeHammer.translateReference(hinputs, hamuts0, innerExprResultType2);
        val (hamuts2, resultType3) = TypeHammer.translateReference(hinputs, hamuts1, resultType2);
        if (innerExprResultType3.kind != Never3()) {
          if (innerExprResultType3 != resultType3) {
            vfail(innerExprResultType3  + " doesnt match " + resultType3);
          }
        }

        val (hamuts3, locals1, stackHeight1, nodesByLine1, innerExprResultLine, deferreds) =
          translate(hinputs, hamuts2, locals0, stackHeight0, nodesByLine0, innerExpr);

        val (nodesByLine2, resultNode) =
          addNode(
            nodesByLine1,
            Reinterpret3(
              newId(nodesByLine1),
              innerExprResultLine.get,
              resultType3))
        val access = RegisterAccess3(resultNode.registerId, resultType3)

        (hamuts3, locals1, stackHeight1, nodesByLine2, Some(access), deferreds)
      }

      case CheckRefCount2(refExpr2, category, numExpr2) => {
        val (hamuts1, locals1, stackHeight1, nodesByLine1, Some(refExprResultLine), refExprDeferreds) =
          translate(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, refExpr2)
        val (hamuts2, refExprResultType3) =
          TypeHammer.translateReference(hinputs, hamuts1, refExpr2.resultRegister.reference);

        val (hamuts3, locals2, stackHeight2, nodesByLine2, Some(numExprResultLine), numExprDeferreds) =
          translate(hinputs, hamuts2, locals1, stackHeight1, nodesByLine1, numExpr2)
        val (hamuts4, numExprResultType3) =
          TypeHammer.translateReference(hinputs, hamuts3, refExpr2.resultRegister.reference);

        val (nodesByLine3, _) =
          addNode(
            nodesByLine2,
            CheckRefCount3(
              newId(nodesByLine2),
              refExprResultLine,
              category,
              numExprResultLine.expectIntAccess()))

        val (hamuts5, locals3, stackHeight3, nodesByLine4) =
          translateDeferreds(hinputs, hamuts4, locals2, stackHeight2, nodesByLine3, numExprDeferreds ++ refExprDeferreds)

        (hamuts5, locals3, stackHeight3, nodesByLine4, None, List())
      }

      case up @ InterfaceToInterfaceUpcast2(innerExpr, targetInterfaceRef2) => {
        val targetPointerType2 = up.resultRegister.reference;
        val sourcePointerType2 = innerExpr.resultRegister.reference

        val (hamuts1, sourcePointerType3) =
          TypeHammer.translateReference(hinputs, hamuts0, sourcePointerType2);
        val (hamuts2, targetPointerType3) =
          TypeHammer.translateReference(hinputs, hamuts1, targetPointerType2);

        val sourceStructRef3 = sourcePointerType3.kind.asInstanceOf[InterfaceRef3]
        val targetInterfaceRef3 = targetPointerType3.kind.asInstanceOf[InterfaceRef3]

        val (hamuts3, locals1, stackHeight1, nodesByLine1, Some(innerExprResultLine), innerDeferreds) =
          translate(hinputs, hamuts2, locals0, stackHeight0, nodesByLine0, innerExpr);
        // Upcasting an interface is technically a no-op with our language, but the sculptor
        // will still want to do it to do some checking in debug mode.
        val (nodesByLine2, upcastNode) =
          addNode(
            nodesByLine1,
            InterfaceToInterfaceUpcast3(
              newId(nodesByLine1),
              innerExprResultLine.expectInterfaceAccess(),
              targetInterfaceRef3));
        val access = RegisterAccess3(upcastNode.registerId, targetPointerType3)
        (hamuts3, locals1, stackHeight1, nodesByLine2, Some(access), innerDeferreds)
      }

      case up @ StructToInterfaceUpcast2(innerExpr, targetInterfaceRef2) => {
        val targetPointerType2 = up.resultRegister.reference;
        val sourcePointerType2 = innerExpr.resultRegister.reference

        val (hamuts1, sourcePointerType3) =
          TypeHammer.translateReference(hinputs, hamuts0, sourcePointerType2);
        val (hamuts2, targetPointerType3) =
          TypeHammer.translateReference(hinputs, hamuts1, targetPointerType2);

        val sourceStructRef3 = sourcePointerType3.kind.asInstanceOf[StructRef3]

        val targetInterfaceRef3 = targetPointerType3.kind.asInstanceOf[InterfaceRef3]

        val (hamuts3, locals1, stackHeight1, nodesByLine1, Some(innerExprResultLine), innerDeferreds) =
          translate(hinputs, hamuts2, locals0, stackHeight0, nodesByLine0, innerExpr);
        // Upcasting an interface is technically a no-op with our language, but the sculptor
        // will still want to do it to do some checking in debug mode.
        val (nodesByLine2, upcastNode) =
          addNode(
            nodesByLine1,
            StructToInterfaceUpcast3(
              newId(nodesByLine1),
              innerExprResultLine.expectStructAccess(),
              targetInterfaceRef3));
        val access = RegisterAccess3(upcastNode.registerId, targetPointerType3)
        (hamuts3, locals1, stackHeight1, nodesByLine2, Some(access), innerDeferreds)
      }

      case ExternFunctionCall2(prototype2, argsExprs2) => {
        val (hamuts5, locals1, stackHeight1, nodesByLine4, access) =
          CallHammer.translateExternFunctionCall(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, prototype2, argsExprs2)
        (hamuts5, locals1, stackHeight1, nodesByLine4, access, List())
      }

      case while2 @ While2(_) => {
        val (hamuts11, locals5, stackHeight1, nodesByLine11) =
          CallHammer.translateWhile(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, while2)
        (hamuts11, locals5, stackHeight1, nodesByLine11, None, List())
      }

      case Defer2(innerExpr, deferredExpr) => {
        val (hamuts1, locals1, stackHeight1, nodesByLine1, innerExprResultLine, innerDeferreds) =
          translate(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, innerExpr);
        (hamuts1, locals1, stackHeight1, nodesByLine1, innerExprResultLine, deferredExpr :: innerDeferreds)
      }

      case Discard2(innerExpr) => {
        val (hamuts1, locals1, stackHeight1, nodesByLine1, Some(innerExprResultLine), innerDeferreds) =
          translate(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, innerExpr);
        vassert(innerDeferreds.isEmpty) // curiosity assert

        innerExprResultLine.expectedType.ownership match {
          case Borrow | Share =>
          case Own => {
            vfail("Owns can only be discarded via destructuring!")
          }
        }

        val (nodesByLine2, discardNode) =
          addNode(
            nodesByLine1,
            Discard3(
              newId(nodesByLine1),
              innerExprResultLine));

        val (hamuts4, locals2, stackHeight2, nodesByLine9) =
          translateDeferreds(hinputs, hamuts1, locals1, stackHeight1, nodesByLine2, innerDeferreds)

        (hamuts4, locals2, stackHeight2, nodesByLine9, None, List())
      }
      case Return2(innerExpr) => {
        val (hamuts1, type3) =
          TypeHammer.translateReference(hinputs, hamuts0, innerExpr.resultRegister.reference)
        val (hamuts2, locals2, stackHeight2, nodesByLine2, Some(innerExprResultLine), innerDeferreds) =
          translate(hinputs, hamuts1, locals0, stackHeight0, nodesByLine0, innerExpr);
        vcurious(innerDeferreds.isEmpty)

        val (nodesByLine3, returnNode) =
          addNode(
            nodesByLine2,
            Return3(
              newId(nodesByLine2),
              innerExprResultLine));
        val access = RegisterAccess3(returnNode.registerId, type3)

        val (hamuts4, locals3, stackHeight3, nodesByLine9) =
          translateDeferreds(hinputs, hamuts2, locals2, stackHeight2, nodesByLine3, innerDeferreds)

        (hamuts4, locals3, stackHeight3, nodesByLine9, Some(access), List())
      }
      case ArgLookup2(paramIndex, type2) => {
        val (hamuts1, type3) =
          TypeHammer.translateReference(hinputs, hamuts0, type2)
        val (nodesByLine1, argNode) =
          ExpressionHammer.addNode(
            nodesByLine0,
            Argument3(newId(nodesByLine0), type3, paramIndex))
        val access = RegisterAccess3(argNode.registerId, type3)
        (hamuts1, locals0, stackHeight0, nodesByLine1, Some(access), List())
      }

      case das2 @ DestroyArraySequence2(_, _, _) => {
        val (hamuts11, locals5, stackHeight1, nodesByLine11) =
          CallHammer.translateDestroyArraySequence(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, das2)
        (hamuts11, locals5, stackHeight1, nodesByLine11, None, List())
      }

      case das2 @ DestroyUnknownSizeArray2(_, _, _) => {
        val (hamuts11, locals5, stackHeight1, nodesByLine11) =
          CallHammer.translateDestroyUnknownSizeArray(
            hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, das2)
        (hamuts11, locals5, stackHeight1, nodesByLine11, None, List())
      }

      case Placeholder2(_) => {
        // So we don't accidentally translate a placeholder, the caller should make sure this never
        // gets here. For example, ConstructArray2 should not try to translate its generator's last arg.
        vfail("Placeholder found!")
      }

      case _ => {
        vfail("wat " + expr2)
      }
    }
  }

  def translateDeferreds(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      deferreds: List[Expression2]):
  (Hamuts, Locals, StackHeight, Vector[Node3]) = {
    val (hamuts3, locals3, stackHeight1, nodesByLine3, deferredsResultLines, deferredDeferreds) =
      translateMaybeReturningExpressions(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, deferreds)
    if (!deferredsResultLines.forall(_.isEmpty)) {
      // curiosity, why would a deferred ever have a result
      vfail("ehwot?")
    }
    if (locals3.locals.size != locals0.locals.size) {
      // There shouldnt have been any locals introduced
      vfail("wat")
    }
    vassert(deferredDeferreds.isEmpty)
    // Don't need these, they should all be voids anyway
    val _ = deferredsResultLines
    (hamuts3, locals3, stackHeight1, nodesByLine3)
  }

  def translateMaybeReturningExpressions(
      hinputs: Hinputs, hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      exprs2: List[Expression2]):
  (Hamuts, Locals, StackHeight, Vector[Node3], List[Option[RegisterAccess3[Referend3]]], List[Expression2]) = {
    exprs2 match {
      case Nil => (hamuts0, locals0, stackHeight0, nodesByLine0, List(), List())
      case firstExpr :: restExprs => {
        val (hamuts1, locals1, stackHeight1, nodesByLine1, firstResultLine, firstDeferreds) =
          translate(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, firstExpr);
        val (hamuts2, locals2, stackHeight2, nodesByLine2, restResultLines, restDeferreds) =
          translateMaybeReturningExpressions(hinputs, hamuts1, locals1, stackHeight1, nodesByLine1, restExprs);

        val resultLines = firstResultLine :: restResultLines
        (hamuts2, locals2, stackHeight2, nodesByLine2, resultLines, restDeferreds ++ firstDeferreds)
      }
    }
  }

  def translateExpressions(
      hinputs: Hinputs, hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      exprs2: List[Expression2]):
  (Hamuts, Locals, StackHeight, Vector[Node3], List[RegisterAccess3[Referend3]], List[Expression2]) = {
    exprs2 match {
      case Nil => (hamuts0, locals0, stackHeight0, nodesByLine0, List(), List())
      case firstExpr :: restExprs => {
        val (hamuts1, locals1, stackHeight1, nodesByLine1, Some(firstResultLine), firstDeferreds) =
          translate(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, firstExpr);
        val (hamuts2, locals2, stackHeight2, nodesByLine2, restResultLines, restDeferreds) =
          translateExpressions(hinputs, hamuts1, locals1, stackHeight1, nodesByLine1, restExprs);

        val resultLines = firstResultLine :: restResultLines
        (hamuts2, locals2, stackHeight2, nodesByLine2, resultLines, restDeferreds ++ firstDeferreds)
      }
    }
  }

  def addNode[T <: Node3](nodesByLine: Vector[Node3], node: T): (Vector[Node3], T) = {
    (nodesByLine :+ node, node)
  }

  def makeEmptyPackStruct(hinputs: Hinputs, hamuts0: Hamuts, nodesByLine0: Vector[Node3]):
  (Hamuts, Vector[Node3], RegisterAccess3[StructRef3]) = {
    val emptyPackType = PackTemplar.emptyPackType
    val (hamuts1, underlyingStructRef3) =
      StructHammer.translateStructRef(hinputs, hamuts0, emptyPackType.underlyingStruct);
    val reference =
      hinputs.program2.lookupStruct(emptyPackType.underlyingStruct).mutability match {
        case Mutable => Reference3(Own, underlyingStructRef3)
        case Immutable => Reference3(Share, underlyingStructRef3)
      }
    val newEmptyStructNode = NewStruct3(newId(nodesByLine0), List(), reference)
    val (nodesByLine1, _) = addNode(nodesByLine0, newEmptyStructNode);
    (hamuts1, nodesByLine1, RegisterAccess3(newEmptyStructNode.registerId, reference))
  }
}