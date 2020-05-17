package net.verdagon.vale.hammer

import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.{metal => m}
import net.verdagon.vale.metal.{ShareH, BorrowH => _, Immutable => _, Mutable => _, OwnH => _, _}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.AddressibleLocalVariable2
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{vassert, vassertSome, vcurious, vfail}

object ExpressionHammer {

  // stackHeight is the number of locals that have been declared in ancestor
  // blocks and previously in this block. It's used to figure out the index of
  // a newly declared local.
  // Returns:
  // - result register id
  // - deferred expressions, to move to after the enclosing call. head is put first after call.
  def translate(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      expr2: Expression2
  ): (Option[RegisterAccessH[ReferendH]], List[Expression2]) = {
    expr2 match {
      case IntLiteral2(value) => {
        val resultNode =
          nodesByLine.addNode(ConstantI64H(nodesByLine.nextId(), value));
        val access = RegisterAccessH[ReferendH](resultNode.registerId, ReferenceH(m.ShareH, IntH()))
        (Some(access), List())
      }
      case VoidLiteral2() => {
        val resultNode = nodesByLine.addNode(ConstantVoidH(nodesByLine.nextId()));
        val access = RegisterAccessH[ReferendH](resultNode.registerId, ReferenceH(m.ShareH, VoidH()))
        (Some(access), List())
      }
      case NeverLiteral2() => {
        val resultNode = nodesByLine.addNode(UnreachableH(nodesByLine.nextId()));
        val access = RegisterAccessH[ReferendH](resultNode.registerId, ReferenceH(m.ShareH, NeverH()))
        (Some(access), List())
      }
      case StrLiteral2(value) => {
        val resultNode =
          nodesByLine.addNode(ConstantStrH(nodesByLine.nextId(), value));
        val access = RegisterAccessH[ReferendH](resultNode.registerId, ReferenceH(m.ShareH, StrH()))
        (Some(access), List())
      }
      case FloatLiteral2(value) => {
        val resultNode =
            nodesByLine.addNode(ConstantF64H(nodesByLine.nextId(), value));
        val access = RegisterAccessH[ReferendH](resultNode.registerId, ReferenceH(m.ShareH, FloatH()))
        (Some(access), List())
      }
      case BoolLiteral2(value) => {
        val resultNode =
            nodesByLine.addNode(ConstantBoolH(nodesByLine.nextId(), value));
        val access = RegisterAccessH[ReferendH](resultNode.registerId, ReferenceH(m.ShareH, BoolH()))
        (Some(access), List())
      }
      case let2 @ LetNormal2(_, _) => {
        LetHammer.translateLet(hinputs, hamuts, locals, stackHeight, nodesByLine, let2)

        val voidResultNode = nodesByLine.addNode(ConstantVoidH(nodesByLine.nextId()));
        val voidAccess = RegisterAccessH[ReferendH](voidResultNode.registerId, ReferenceH(m.ShareH, VoidH()))
        (Some(voidAccess), List())
      }
      case let2 @ LetAndLend2(_, _) => {
        val borrowAccess =
          LetHammer.translateLetAndLend(hinputs, hamuts, locals, stackHeight, nodesByLine, let2)
        (Some(borrowAccess), List())
      }
      case des2 @ Destructure2(_, _, _) => {
        LetHammer.translateDestructure(hinputs, hamuts, locals, stackHeight, nodesByLine, des2)
        // Templar destructures put things in local variables (even though hammer itself
        // uses registers internally to make that happen).
        // Since all the members landed in locals, we still need something to return, so we
        // return a void.
        val voidResultNode = nodesByLine.addNode(ConstantVoidH(nodesByLine.nextId()));
        val voidAccess = RegisterAccessH[ReferendH](voidResultNode.registerId, ReferenceH(m.ShareH, VoidH()))
        (Some(voidAccess), List())
      }
      case des2 @ DestructureArraySequence2(_, _, _) => {
        LetHammer.translateDestructureArraySequence(hinputs, hamuts, locals, stackHeight, nodesByLine, des2)
        // Templar destructures put things in local variables (even though hammer itself
        // uses registers internally to make that happen).
        // Since all the members landed in locals, we still need something to return, so we
        // return a void.
        val voidResultNode = nodesByLine.addNode(ConstantVoidH(nodesByLine.nextId()));
        val voidAccess = RegisterAccessH[ReferendH](voidResultNode.registerId, ReferenceH(m.ShareH, VoidH()))
        (Some(voidAccess), List())
      }
      case unlet2 @ Unlet2(_) => {
        val valueAccess =
          LetHammer.translateUnlet(
            hinputs, hamuts, locals, stackHeight, nodesByLine, unlet2)
        (Some(valueAccess), List())
      }
      case mutate2 @ Mutate2(_, _) => {
        val newEmptyPackStructNodeLine =
          MutateHammer.translateMutate(hinputs, hamuts, locals, stackHeight, nodesByLine, mutate2)
        (Some(newEmptyPackStructNodeLine), List())
      }
      case b @ Block2(_) => {
        val stackHeightForBlock = StackHeightBox(stackHeight.snapshot.oneBlockHigher())
        val (blockH, maybeBlockResultAccess) =
          BlockHammer.translateBlock(hinputs, hamuts, locals.snapshot, stackHeightForBlock, b)
        val inlineBlockResultNode =
          nodesByLine.addNode(InlineBlockH(nodesByLine.nextId(), blockH));
        val maybeAccess =
          maybeBlockResultAccess match {
            case None => None
            case Some(access) => {
              Some(RegisterAccessH[ReferendH](inlineBlockResultNode.registerId, access.expectedType))
            }
          }
        (maybeAccess, List())
      }
      case call2 @ FunctionCall2(callableExpr, args) => {
        val access =
          CallHammer.translateFunctionPointerCall(
            hinputs, hamuts, locals, stackHeight, nodesByLine, callableExpr, args, call2.resultRegister.reference)
        (Some(access), List())
      }

      case InterfaceFunctionCall2(superFunctionHeader, resultType2, argsExprs2) => {
        val access =
          CallHammer.translateInterfaceFunctionCall(
            hinputs, hamuts, locals, stackHeight, nodesByLine, superFunctionHeader, resultType2, argsExprs2)
        (Some(access), List())
      }

      case Consecutor2(exprs) => {
        val (resultLines, deferreds) =
          translateMaybeReturningExpressions(hinputs, hamuts, locals, stackHeight, nodesByLine, exprs);
        resultLines.init.foreach({
          case None =>
          case Some(nonLastResultLine) => vassert(nonLastResultLine.expectedType.kind == VoidH())
        })
        vassert(deferreds.isEmpty) // curiosity, would we have any here?
        (resultLines.last, List())
      }

      case PackE2(exprs, resultType, resultPackType) => {
        val (resultLines, deferreds) =
          translateExpressions(hinputs, hamuts, locals, stackHeight, nodesByLine, exprs);
        val (underlyingStructRefH) =
          StructHammer.translateStructRef(hinputs, hamuts, resultPackType.underlyingStruct);
        val (resultReference) =
          TypeHammer.translateReference(hinputs, hamuts, resultType)
        vassert(resultReference.kind == underlyingStructRefH)

        val structDefH = hamuts.structDefsByRef2(resultPackType.underlyingStruct)
        vassert(resultLines.size == structDefH.members.size)
        val newStructNode =
          NewStructH(
            nodesByLine.nextId(),
            resultLines,
            resultReference.expectStructReference())

        val _ = nodesByLine.addNode(newStructNode);


        translateDeferreds(hinputs, hamuts, locals, stackHeight, nodesByLine, deferreds)

        val access = RegisterAccessH(newStructNode.registerId, resultReference)
        // Export locals from inside the pack
        (Some(access), List())
      }

      case ArrayLength2(arrayExpr2) => {
        val (resultLine, deferreds) =
          translate(hinputs, hamuts, locals, stackHeight, nodesByLine, arrayExpr2);

        val lengthResultNode =
          nodesByLine.addNode(ArrayLengthH(nodesByLine.nextId(), vassertSome(resultLine)));

        translateDeferreds(hinputs, hamuts, locals, stackHeight, nodesByLine, deferreds)

        val access = RegisterAccessH[ReferendH](lengthResultNode.registerId, ReferenceH(m.ShareH, IntH()))
        (Some(access), List())
      }

      case TupleE2(exprs, resultType, resultPackType) => {
        val (resultLines, deferreds) =
          translateExpressions(hinputs, hamuts, locals, stackHeight, nodesByLine, exprs);
        val (underlyingStructRefH) =
          StructHammer.translateStructRef(hinputs, hamuts, resultPackType.underlyingStruct);
        val (resultReference) =
          TypeHammer.translateReference(hinputs, hamuts, resultType)
        vassert(resultReference.kind == underlyingStructRefH)

        val structDefH = hamuts.structDefsByRef2(resultPackType.underlyingStruct)
        vassert(resultLines.size == structDefH.members.size)
        val newStructNode =
          NewStructH(
            nodesByLine.nextId(),
            resultLines,
            resultReference.expectStructReference())
        val _ = nodesByLine.addNode(newStructNode);
        // Export locals from inside the pack


        translateDeferreds(hinputs, hamuts, locals, stackHeight, nodesByLine, deferreds)

        val access = RegisterAccessH(newStructNode.registerId, resultReference)
        (Some(access), List())
      }

      case ArraySequenceE2(exprs, arrayReference2, arrayType2) => {
        val (resultLines, deferreds) =
          translateExpressions(hinputs, hamuts, locals, stackHeight, nodesByLine, exprs);
        val (underlyingArrayH) =
          TypeHammer.translateKnownSizeArray(hinputs, hamuts, arrayType2);

        val (arrayReferenceH) =
          TypeHammer.translateReference(hinputs, hamuts, arrayReference2)
        vassert(arrayReferenceH.kind == underlyingArrayH)

        val newStructNode =
          NewArrayFromValuesH(
            nodesByLine.nextId(),
            resultLines,
            arrayReferenceH.expectKnownSizeArrayReference())
        val _ = nodesByLine.addNode(newStructNode);


        translateDeferreds(hinputs, hamuts, locals, stackHeight, nodesByLine, deferreds)

        val access = RegisterAccessH(newStructNode.registerId, arrayReferenceH)
        (Some(access), List())
      }

      case Construct2(structRef2, resultType2, memberExprs) => {
        val (memberResultLines, deferreds) =
          translateExpressions(hinputs, hamuts, locals, stackHeight, nodesByLine, memberExprs);

        val (resultTypeH) =
          TypeHammer.translateReference(hinputs, hamuts, resultType2)

//        hinputs.program2.lookupStruct(resultStructType2)
//        vassert(structDef2.getRef == resultTypeH.innerType)

        val structDefH = hamuts.structDefsByRef2(structRef2)
        vassert(memberResultLines.size == structDefH.members.size)
        val newStructNode =
          NewStructH(
            nodesByLine.nextId(),
            memberResultLines,
            resultTypeH.expectStructReference())

        val _ = nodesByLine.addNode(newStructNode);

        translateDeferreds(hinputs, hamuts, locals, stackHeight, nodesByLine, deferreds)

        val access = RegisterAccessH(newStructNode.registerId, resultTypeH)
        (Some(access), List())
      }

      case load2 @ SoftLoad2(_, _) => {
        val (loadedAccessH, deferreds) =
          LoadHammer.translateLoad(hinputs, hamuts, locals, stackHeight, nodesByLine, load2)
        (Some(loadedAccessH), deferreds)
      }

      case lookup2 @ LocalLookup2(AddressibleLocalVariable2(_, _, _), _) => {
        val loadBoxAccess =
          LoadHammer.translateLocalAddress(hinputs, hamuts, locals, stackHeight, nodesByLine, lookup2)
        (Some(loadBoxAccess), List())
      }

      case lookup2 @ AddressMemberLookup2(_, _, _) => {
        val (loadBoxAccess, deferreds) =
          LoadHammer.translateMemberAddress(hinputs, hamuts, locals, stackHeight, nodesByLine, lookup2)
        (Some(loadBoxAccess), deferreds)
      }

      case if2 @ If2(_, _, _) => {
        val maybeAccess =
          CallHammer.translateIf(hinputs, hamuts, locals, stackHeight, nodesByLine, if2)
        (maybeAccess, List())
      }

      case ca2 @ ConstructArray2(_, _, _) => {
        val access =
          CallHammer.translateConstructArray(
            hinputs, hamuts, locals, stackHeight, nodesByLine, ca2)
        (Some(access), List())
      }

      case TemplarReinterpret2(innerExpr, resultType2) => {
        // Check types; it's overkill because reinterprets are rather scary.
        val innerExprResultType2 = innerExpr.resultRegister.reference
        val (innerExprResultTypeH) = TypeHammer.translateReference(hinputs, hamuts, innerExprResultType2);
        val (resultTypeH) = TypeHammer.translateReference(hinputs, hamuts, resultType2);
        if (innerExprResultTypeH.kind != NeverH()) {
          if (innerExprResultTypeH != resultTypeH) {
            vfail(innerExprResultTypeH  + " doesnt match " + resultTypeH);
          }
        }

        val (innerExprResultLine, deferreds) =
          translate(hinputs, hamuts, locals, stackHeight, nodesByLine, innerExpr);

        // Not always the case:
        //   vcurious(innerExprResultTypeH.kind == NeverH() || resultTypeH.kind == NeverH())
        // for example when we're destructuring a TupleT2 or PackT2, we interpret from that
        // to its understruct.

        // These both trip:
        //   vcurious(innerExprResultTypeH == resultTypeH)
        //   vcurious(innerExprResultTypeH != resultTypeH)
        // Because sometimes theyre actually the same, because we might interpret a tuple to
        // its understruct, and sometimes theyre different, when we're making a Never into
        // an Int for example when one branch of an If panics or returns.

        if (innerExprResultTypeH.kind == NeverH() || resultTypeH.kind == NeverH()) {
          val resultNode =
            nodesByLine.addNode(
              UnreachableH(
                nodesByLine.nextId()))
          val access = RegisterAccessH(resultNode.registerId, resultTypeH)
          (Some(access), deferreds)
        } else {
          (innerExprResultLine, deferreds)
        }
      }

      case CheckRefCount2(refExpr2, category, numExpr2) => {
        val (Some(refExprResultLine), refExprDeferreds) =
          translate(hinputs, hamuts, locals, stackHeight, nodesByLine, refExpr2)
        val (refExprResultTypeH) =
          TypeHammer.translateReference(hinputs, hamuts, refExpr2.resultRegister.reference);

        val (Some(numExprResultLine), numExprDeferreds) =
          translate(hinputs, hamuts, locals, stackHeight, nodesByLine, numExpr2)
        val (numExprResultTypeH) =
          TypeHammer.translateReference(hinputs, hamuts, refExpr2.resultRegister.reference);

        val _ =
          nodesByLine.addNode(
            CheckRefCountH(
              nodesByLine.nextId(),
              refExprResultLine,
              Conversions.evaluateRefCountCategory(category),
              numExprResultLine.expectIntAccess()))


        translateDeferreds(hinputs, hamuts, locals, stackHeight, nodesByLine, numExprDeferreds ++ refExprDeferreds)

        val voidResultNode = nodesByLine.addNode(ConstantVoidH(nodesByLine.nextId()));
        val voidAccess = RegisterAccessH[ReferendH](voidResultNode.registerId, ReferenceH(m.ShareH, VoidH()))
        (Some(voidAccess), List())
      }

      case up @ InterfaceToInterfaceUpcast2(innerExpr, targetInterfaceRef2) => {
        val targetPointerType2 = up.resultRegister.reference;
        val sourcePointerType2 = innerExpr.resultRegister.reference

        val (sourcePointerTypeH) =
          TypeHammer.translateReference(hinputs, hamuts, sourcePointerType2);
        val (targetPointerTypeH) =
          TypeHammer.translateReference(hinputs, hamuts, targetPointerType2);

        val sourceStructRefH = sourcePointerTypeH.kind.asInstanceOf[InterfaceRefH]
        val targetInterfaceRefH = targetPointerTypeH.kind.asInstanceOf[InterfaceRefH]

        val (Some(innerExprResultLine), innerDeferreds) =
          translate(hinputs, hamuts, locals, stackHeight, nodesByLine, innerExpr);
        // Upcasting an interface is technically a no-op with our language, but the sculptor
        // will still want to do it to do some checking in debug mode.
        val upcastNode =
          nodesByLine.addNode(
            InterfaceToInterfaceUpcastH(
              nodesByLine.nextId(),
              innerExprResultLine.expectInterfaceAccess(),
              targetInterfaceRefH));
        val access = RegisterAccessH(upcastNode.registerId, targetPointerTypeH)
        (Some(access), innerDeferreds)
      }

      case up @ StructToInterfaceUpcast2(innerExpr, targetInterfaceRef2) => {
        val targetPointerType2 = up.resultRegister.reference;
        val sourcePointerType2 = innerExpr.resultRegister.reference

        val (sourcePointerTypeH) =
          TypeHammer.translateReference(hinputs, hamuts, sourcePointerType2);
        val (targetPointerTypeH) =
          TypeHammer.translateReference(hinputs, hamuts, targetPointerType2);

        val sourceStructRefH = sourcePointerTypeH.kind.asInstanceOf[StructRefH]

        val targetInterfaceRefH = targetPointerTypeH.kind.asInstanceOf[InterfaceRefH]

        val (Some(innerExprResultLine), innerDeferreds) =
          translate(hinputs, hamuts, locals, stackHeight, nodesByLine, innerExpr);
        // Upcasting an interface is technically a no-op with our language, but the sculptor
        // will still want to do it to do some checking in debug mode.
        val upcastNode =
          nodesByLine.addNode(
            StructToInterfaceUpcastH(
              nodesByLine.nextId(),
              innerExprResultLine.expectStructAccess(),
              targetInterfaceRefH));
        val access = RegisterAccessH(upcastNode.registerId, targetPointerTypeH)
        (Some(access), innerDeferreds)
      }

      case ExternFunctionCall2(prototype2, argsExprs2) => {
        val access =
          CallHammer.translateExternFunctionCall(hinputs, hamuts, locals, stackHeight, nodesByLine, prototype2, argsExprs2)
        (access, List())
      }

      case while2 @ While2(_) => {
        CallHammer.translateWhile(hinputs, hamuts, locals, stackHeight, nodesByLine, while2)

        val voidResultNode = nodesByLine.addNode(ConstantVoidH(nodesByLine.nextId()));
        val voidAccess = RegisterAccessH[ReferendH](voidResultNode.registerId, ReferenceH(m.ShareH, VoidH()))
        (Some(voidAccess), List())
      }

      case Defer2(innerExpr, deferredExpr) => {
        val (innerExprResultLine, innerDeferreds) =
          translate(hinputs, hamuts, locals, stackHeight, nodesByLine, innerExpr);
        (innerExprResultLine, deferredExpr :: innerDeferreds)
      }

      case Discard2(innerExpr) => {
        val (maybeInnerExprResultLine, innerDeferreds) =
          translate(hinputs, hamuts, locals, stackHeight, nodesByLine, innerExpr);
        val innerExprResultLine = vassertSome(maybeInnerExprResultLine)
        vassert(innerDeferreds.isEmpty) // curiosity assert

        innerExprResultLine.expectedType.ownership match {
          case m.BorrowH | m.ShareH =>
          case m.OwnH => {
            vfail("Owns can only be discarded via destructuring!")
          }
        }

        // We don't discard Void, see VDND.
        if (innerExpr.resultRegister.reference.referend != Void2()) {
          nodesByLine.addNode(
            DiscardH(
              nodesByLine.nextId(),
              innerExprResultLine));
        }

        translateDeferreds(hinputs, hamuts, locals, stackHeight, nodesByLine, innerDeferreds)

        (None, List())
      }
      case Return2(innerExpr) => {
        val (Some(innerExprResultLine), innerDeferreds) =
          translate(hinputs, hamuts, locals, stackHeight, nodesByLine, innerExpr);
        vcurious(innerDeferreds.isEmpty)

        nodesByLine.addNode(
          ReturnH(
            nodesByLine.nextId(),
            innerExprResultLine));

        translateDeferreds(hinputs, hamuts, locals, stackHeight, nodesByLine, innerDeferreds)

        val resultNode =
          nodesByLine.addNode(
            UnreachableH(
              nodesByLine.nextId()))
        val access = RegisterAccessH(resultNode.registerId, ReferenceH(ShareH, NeverH()))
        (Some(access), List())
      }
      case ArgLookup2(paramIndex, type2) => {
        val (typeH) =
          TypeHammer.translateReference(hinputs, hamuts, type2)
        val argNode =
          nodesByLine.addNode(
            ArgumentH(nodesByLine.nextId(), typeH, paramIndex))
        val access = RegisterAccessH(argNode.registerId, typeH)
        (Some(access), List())
      }

      case das2 @ DestroyArraySequence2(_, _, _) => {
        CallHammer.translateDestroyArraySequence(hinputs, hamuts, locals, stackHeight, nodesByLine, das2)

        val voidResultNode = nodesByLine.addNode(ConstantVoidH(nodesByLine.nextId()));
        val voidAccess = RegisterAccessH[ReferendH](voidResultNode.registerId, ReferenceH(m.ShareH, VoidH()))
        (Some(voidAccess), List())
      }

      case das2 @ DestroyUnknownSizeArray2(_, _, _) => {
        CallHammer.translateDestroyUnknownSizeArray(
          hinputs, hamuts, locals, stackHeight, nodesByLine, das2)

        val voidResultNode = nodesByLine.addNode(ConstantVoidH(nodesByLine.nextId()));
        val voidAccess = RegisterAccessH[ReferendH](voidResultNode.registerId, ReferenceH(m.ShareH, VoidH()))
        (Some(voidAccess), List())
      }

      case _ => {
        vfail("wat " + expr2)
      }
    }
  }

  def translateDeferreds(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      deferreds: List[Expression2]):
  Unit = {
    val (deferredsResultLines, deferredDeferreds) =
      translateMaybeReturningExpressions(hinputs, hamuts, locals, stackHeight, nodesByLine, deferreds)
    if (!deferredsResultLines.forall(_.isEmpty)) {
      // curiosity, why would a deferred ever have a result
      vfail("ehwot?")
    }
    if (locals.locals.size != locals.locals.size) {
      // There shouldnt have been any locals introduced
      vfail("wat")
    }
    vassert(deferredDeferreds.isEmpty)
    // Don't need these, they should all be voids anyway
    val _ = deferredsResultLines
  }

  def translateMaybeReturningExpressions(
      hinputs: Hinputs, hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      exprs2: List[Expression2]):
  (List[Option[RegisterAccessH[ReferendH]]], List[Expression2]) = {
    exprs2 match {
      case Nil => (List(), List())
      case firstExpr :: restExprs => {
        val (firstResultLine, firstDeferreds) =
          translate(hinputs, hamuts, locals, stackHeight, nodesByLine, firstExpr);
        val (restResultLines, restDeferreds) =
          translateMaybeReturningExpressions(hinputs, hamuts, locals, stackHeight, nodesByLine, restExprs);

        val resultLines = firstResultLine :: restResultLines
        (resultLines, restDeferreds ++ firstDeferreds)
      }
    }
  }

  def translateExpressions(
      hinputs: Hinputs, hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      exprs2: List[Expression2]):
  (List[RegisterAccessH[ReferendH]], List[Expression2]) = {
    exprs2 match {
      case Nil => (List(), List())
      case firstExpr :: restExprs => {
        val (Some(firstResultLine), firstDeferreds) =
          translate(hinputs, hamuts, locals, stackHeight, nodesByLine, firstExpr);
        val (restResultLines, restDeferreds) =
          translateExpressions(hinputs, hamuts, locals, stackHeight, nodesByLine, restExprs);

        val resultLines = firstResultLine :: restResultLines
        (resultLines, restDeferreds ++ firstDeferreds)
      }
    }
  }

  def makeEmptyPackStruct(hinputs: Hinputs, hamuts: HamutsBox, nodesByLine: NodesBox):
  RegisterAccessH[StructRefH] = {
    val emptyPackType = Program2.emptyPackType
    val (underlyingStructRefH) =
      StructHammer.translateStructRef(hinputs, hamuts, emptyPackType.underlyingStruct);
    val reference =
      hinputs.program2.lookupStruct(emptyPackType.underlyingStruct).mutability match {
        case Mutable => ReferenceH(m.OwnH, underlyingStructRefH)
        case Immutable => ReferenceH(m.ShareH, underlyingStructRefH)
      }
    val newEmptyStructNode = NewStructH(nodesByLine.nextId(), List(), reference)
    val _ = nodesByLine.addNode(newEmptyStructNode);
    RegisterAccessH(newEmptyStructNode.registerId, reference)
  }
}
