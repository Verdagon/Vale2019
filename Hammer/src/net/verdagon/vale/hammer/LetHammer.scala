package net.verdagon.vale.hammer

import net.verdagon.vale.hammer.ExpressionHammer.translate
import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.{vassertSome, vfail, metal => m}
import net.verdagon.vale.metal.{Variability => _, _}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.{AddressibleLocalVariable2, ReferenceLocalVariable2}
import net.verdagon.vale.templar.types._

object LetHammer {

  def translateLet(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      let2: LetNormal2):
  Unit = {
    val LetNormal2(localVariable, sourceExpr2) = let2

    val (maybeSourceExprResultLine, deferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, sourceExpr2);
    val sourceExprResultLine = vassertSome(maybeSourceExprResultLine)
    val (sourceResultPointerTypeH) =
      TypeHammer.translateReference(hinputs, hamuts, sourceExpr2.resultRegister.reference)

    localVariable match {
      case ReferenceLocalVariable2(varId, variability, type2) => {
        translateMundaneLet(
          hinputs, hamuts, locals, stackHeight, nodesByLine, sourceExprResultLine, sourceResultPointerTypeH, varId)
      }
      case AddressibleLocalVariable2(varId, variability, reference) => {
        translateAddressibleLet(
          hinputs, hamuts, locals, stackHeight, nodesByLine, sourceExprResultLine, sourceResultPointerTypeH, varId, variability, reference)
      }
    }

    ExpressionHammer.translateDeferreds(
      hinputs, hamuts, locals, stackHeight, nodesByLine, deferreds)
  }

  def translateLetAndLend(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      let2: LetAndLend2):
  (RegisterAccessH[ReferendH]) = {
    val LetAndLend2(localVariable, sourceExpr2) = let2

    val (Some(sourceExprResultLine), deferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, sourceExpr2);
    val (sourceResultPointerTypeH) =
      TypeHammer.translateReference(hinputs, hamuts, sourceExpr2.resultRegister.reference)

    val borrowAccess =
      localVariable match {
        case ReferenceLocalVariable2(varId, variability, type2) => {
          translateMundaneLetAndLend(
            hinputs, hamuts, locals, stackHeight, nodesByLine, sourceExpr2, sourceExprResultLine, sourceResultPointerTypeH, let2, varId)
        }
        case AddressibleLocalVariable2(varId, variability, reference) => {
          translateAddressibleLetAndLend(
            hinputs, hamuts, locals, stackHeight, nodesByLine, sourceExpr2, sourceExprResultLine, sourceResultPointerTypeH, let2, varId, variability, reference)
        }
      }


      ExpressionHammer.translateDeferreds(
        hinputs, hamuts, locals, stackHeight, nodesByLine, deferreds)

    (borrowAccess)
  }

  private def translateAddressibleLet(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      sourceExprResultLine: RegisterAccessH[ReferendH],
      sourceResultPointerTypeH: ReferenceH[ReferendH],
      varId: FullName2[IVarName2],
      variability: Variability,
      reference: Coord):
  Unit = {
    val (boxStructRefH) =
      StructHammer.makeBox(hinputs, hamuts, variability, reference, sourceResultPointerTypeH)
    val expectedLocalBoxType = ReferenceH(m.OwnH, boxStructRefH)

    val local =
      locals.addTemplarLocal(hinputs, hamuts, varId, stackHeight.snapshot, expectedLocalBoxType)
    stackHeight.oneLocalHigher()
    val boxNode =
      nodesByLine.addNode(
        NewStructH(
          nodesByLine.nextId(),
          List(sourceExprResultLine),
          expectedLocalBoxType))
    val stackNode =
      nodesByLine.addNode(
        StackifyH(
          nodesByLine.nextId(),
          RegisterAccessH(boxNode.registerId, expectedLocalBoxType),
          local,
          NameHammer.translateFullName(hinputs, hamuts, varId)))
    val _ = stackNode // Don't need it
  }

  private def translateAddressibleLetAndLend(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      sourceExpr2: ReferenceExpression2,
      sourceExprResultLine: RegisterAccessH[ReferendH],
      sourceResultPointerTypeH: ReferenceH[ReferendH],
      let2: LetAndLend2,
      varId: FullName2[IVarName2],
      variability: Variability,
      reference: Coord):
  (RegisterAccessH[ReferendH]) = {

      translateAddressibleLet(
        hinputs, hamuts, locals, stackHeight, nodesByLine, sourceExprResultLine, sourceResultPointerTypeH, varId, variability, reference)
    val (borrowAccess, List()) =
      LoadHammer.translateAddressibleLocalLoad(
        hinputs,
        hamuts,
        locals,
        stackHeight,
        nodesByLine,
        varId,
        variability,
        sourceExpr2.resultRegister.reference,
        let2.resultRegister.reference.ownership)
    (borrowAccess)
  }

  private def translateMundaneLet(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      sourceExprResultLine: RegisterAccessH[ReferendH],
      sourceResultPointerTypeH: ReferenceH[ReferendH],
      varId: FullName2[IVarName2]):
  Unit = {
    val localIndex =
      locals.addTemplarLocal(hinputs, hamuts, varId, stackHeight.snapshot, sourceResultPointerTypeH)
    stackHeight.oneLocalHigher()
    val stackNode =
      nodesByLine.addNode(
        StackifyH(
          nodesByLine.nextId(),
          sourceExprResultLine,
          localIndex,
          NameHammer.translateFullName(hinputs, hamuts, varId)))
    val _ = stackNode // Don't need it
  }

    private def translateMundaneLetAndLend(
        hinputs: Hinputs,
        hamuts: HamutsBox,
        locals: LocalsBox,
        stackHeight: StackHeightBox,
        nodesByLine: NodesBox,
        sourceExpr2: ReferenceExpression2,
        sourceExprResultLine: RegisterAccessH[ReferendH],
        sourceResultPointerTypeH: ReferenceH[ReferendH],
        let2: LetAndLend2,
        varId: FullName2[IVarName2]):
    RegisterAccessH[ReferendH] = {

        translateMundaneLet(
          hinputs,
          hamuts,
          locals,
          stackHeight,
          nodesByLine,
          sourceExprResultLine,
          sourceResultPointerTypeH,
          varId)

    val (borrowAccess, List()) =
      LoadHammer.translateMundaneLocalLoad(
        hinputs,
        hamuts,
        locals,
        stackHeight,
        nodesByLine,
        varId,
        sourceExpr2.resultRegister.reference,
        let2.resultRegister.reference.ownership)

      (borrowAccess)
  }

  def translateUnlet(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      unlet2: Unlet2):
  (RegisterAccessH[ReferendH]) = {
    val local =
      locals.get(unlet2.variable.id) match {
        case None => {
          vfail("Unletting an unknown variable: " + unlet2.variable.id)
        }
        case Some(local) => local
      }

    unlet2.variable match {
      case ReferenceLocalVariable2(varId, _, localType2) => {
        val (localTypeH) =
          TypeHammer.translateReference(hinputs, hamuts, localType2)
        val unstackifyNode =
          nodesByLine.addNode(
            UnstackifyH(
              nodesByLine.nextId(),
              local,
              localTypeH))
        val valueAccess =
          RegisterAccessH(unstackifyNode.registerId, localTypeH)
        locals.markUnstackified(varId)
        (valueAccess)
      }
      case AddressibleLocalVariable2(varId, variability, innerType2) => {
        val (innerTypeH) =
          TypeHammer.translateReference(hinputs, hamuts, innerType2)
        val (structRefH) =
          StructHammer.makeBox(hinputs, hamuts, variability, innerType2, innerTypeH)
        val localTypeH = ReferenceH(m.OwnH, structRefH)

        val unstackifyBoxNode =
          nodesByLine.addNode(
            UnstackifyH(
              nodesByLine.nextId(),
              local,
              localTypeH))
        val boxValueAccess =
          RegisterAccessH(unstackifyBoxNode.registerId, localTypeH)
        locals.markUnstackified(varId)

        val innerLocal =
          locals.addHammerLocal(stackHeight.snapshot, innerTypeH)
        stackHeight.oneLocalHigher()

        val _ =
          nodesByLine.addNode(
            DestructureH(
              nodesByLine.nextId(),
              boxValueAccess,
              List(innerTypeH),
              Vector(innerLocal)))
        locals.markUnstackified(innerLocal.id)

        val unstackifyContentsNode =
          nodesByLine.addNode(
            UnstackifyH(
              nodesByLine.nextId(),
              innerLocal,
              innerTypeH))
        val contentsValueAccess =
          RegisterAccessH(unstackifyContentsNode.registerId, innerTypeH)

        (contentsValueAccess)
      }
    }
  }

  def translateDestructure(
      hinputs: Hinputs,
      hamuts: HamutsBox,
      locals: LocalsBox,
      stackHeight: StackHeightBox,
      nodesByLine: NodesBox,
      des2: Destructure2):
  Unit = {
    val Destructure2(sourceExpr2, structRef2, destinationReferenceLocalVariables) = des2

    val (Some(sourceExprResultLine), sourceExprDeferreds) =
      translate(hinputs, hamuts, locals, stackHeight, nodesByLine, sourceExpr2);

    val structDef2 = hinputs.program2.lookupStruct(structRef2)

    // Destructure2 will immediately destroy any addressible references inside it
    // (see Destructure2 comments).
    // In the post-addressible world with all our boxes and stuff, an addressible
    // reference member is actually a borrow reference to a box.
    // Destructure2's destroying of addressible references translates to hammer
    // unborrowing the references to boxes.
    // However, the templar only supplied variables for the reference members,
    // so we need to introduce our own local variables here.

    // We put List() here to make sure that we've consumed all the destination
    // reference local variables.
    val (List(), localTypes, localIndices) =
      structDef2.members.foldLeft((destinationReferenceLocalVariables, List[ReferenceH[ReferendH]](), List[Local]()))({
        case ((remainingDestinationReferenceLocalVariables, previousLocalTypes, previousLocalIndices), member2) => {
          member2.tyype match {
            case ReferenceMemberType2(memberRefType2) => {
              val destinationReferenceLocalVariable = remainingDestinationReferenceLocalVariables.head

              val (memberRefTypeH) =
                TypeHammer.translateReference(hinputs, hamuts, memberRefType2)
              val localIndex =
                locals.addTemplarLocal(
                  hinputs, hamuts, destinationReferenceLocalVariable.id, stackHeight.snapshot, memberRefTypeH)
              stackHeight.oneLocalHigher()
              (remainingDestinationReferenceLocalVariables.tail, previousLocalTypes :+ memberRefTypeH, previousLocalIndices :+ localIndex)
            }
            // The struct might have addressibles in them, which translate to
            // borrow refs of boxes which contain things. We're moving that borrow
            // ref into a local variable. We'll then unlet the local variable, and
            // unborrow it.
            case AddressMemberType2(memberRefType2) => {
              val (memberRefTypeH) =
                TypeHammer.translateReference(hinputs, hamuts, memberRefType2);
              // In the case of an addressible struct member, its variability refers to the
              // variability of the pointee variable, see StructMember2
              val (boxStructRefH) =
                StructHammer.makeBox(hinputs, hamuts, member2.variability, memberRefType2, memberRefTypeH)
              // Structs only ever borrow boxes, boxes are only ever owned by the stack.
              val localBoxType = ReferenceH(m.BorrowH, boxStructRefH)
              val localIndex =
                locals.addHammerLocal(stackHeight.snapshot, localBoxType)
              stackHeight.oneLocalHigher()

              (remainingDestinationReferenceLocalVariables, previousLocalTypes :+ localBoxType, previousLocalIndices :+ localIndex)
            }
          }
        }
      })

    val stackNode =
      nodesByLine.addNode(
        DestructureH(
          nodesByLine.nextId(),
          sourceExprResultLine.expectStructAccess(),
          localTypes,
          localIndices.toVector))
    val _ = stackNode // Don't need it

      structDef2.members.zip(localTypes.zip(localIndices)).foreach({
        case (structMember2, (localType, local)) => {
          structMember2.tyype match {
            case ReferenceMemberType2(_) => (locals, nodesByLine)
            case AddressMemberType2(_) => {
              // localType is the box type.
              // First, unlet it, then discard it.
              val unstackifyNode =
                nodesByLine.addNode(
                  UnstackifyH(
                    nodesByLine.nextId(),
                    local,
                    localType))
              val unstackifiedAccess =
                RegisterAccessH(unstackifyNode.registerId, localType)
              locals.markUnstackified(local.id)

              val discardNode =
                nodesByLine.addNode(
                  DiscardH(
                    nodesByLine.nextId(),
                    unstackifiedAccess))
              val _ = discardNode
            }
          }
        }
      })

      ExpressionHammer.translateDeferreds(
        hinputs, hamuts, locals, stackHeight, nodesByLine, sourceExprDeferreds)
  }
}
