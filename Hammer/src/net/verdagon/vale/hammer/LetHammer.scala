package net.verdagon.vale.hammer

import net.verdagon.vale.hammer.ExpressionHammer.{addNode, newId, translate}
import net.verdagon.vale.hinputs.Hinputs
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.{AddressibleLocalVariable2, ReferenceLocalVariable2, VariableId2}
import net.verdagon.vale.templar.types._
import net.verdagon.vale.vfail

object LetHammer {

  def translateLet(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      let2: LetNormal2):
  (Hamuts, Locals, StackHeight, Vector[NodeH]) = {
    val LetNormal2(localVariable, sourceExpr2) = let2

    val (hamuts1, locals1, stackHeight1, nodesByLine1, Some(sourceExprResultLine), deferreds) =
      translate(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, sourceExpr2);
    val (hamuts2, sourceResultPointerTypeH) =
      TypeHammer.translateReference(hinputs, hamuts1, sourceExpr2.resultRegister.reference)

    val (hamuts4, localsH, stackHeight2, nodesByLineH) =
      localVariable match {
        case ReferenceLocalVariable2(varId, variability, type2) => {
          translateMundaneLet(
            hinputs, hamuts2, locals1, stackHeight1, nodesByLine1, sourceExprResultLine, sourceResultPointerTypeH, varId)
        }
        case AddressibleLocalVariable2(varId, variability, reference) => {
          translateAddressibleLet(
            hinputs, hamuts2, locals1, stackHeight1, nodesByLine1, sourceExprResultLine, sourceResultPointerTypeH, varId, variability, reference)
        }
      }

    val (hamuts5, locals4, stackHeightH, nodesByLine5) =
      ExpressionHammer.translateDeferreds(
        hinputs, hamuts4, localsH, stackHeight2, nodesByLineH, deferreds)

    (hamuts5, locals4, stackHeightH, nodesByLine5)
  }

  def translateLetAndLend(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      let2: LetAndLend2):
  (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH]) = {
    val LetAndLend2(localVariable, sourceExpr2) = let2

    val (hamuts1, locals1, stackHeight1, nodesByLine1, Some(sourceExprResultLine), deferreds) =
      translate(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, sourceExpr2);
    val (hamuts2, sourceResultPointerTypeH) =
      TypeHammer.translateReference(hinputs, hamuts1, sourceExpr2.resultRegister.reference)

    val (hamuts4, localsH, stackHeight2, nodesByLineH, borrowAccess) =
      localVariable match {
        case ReferenceLocalVariable2(varId, variability, type2) => {
          translateMundaneLetAndLend(
            hinputs, hamuts2, locals1, stackHeight1, nodesByLine1, sourceExpr2, sourceExprResultLine, sourceResultPointerTypeH, let2, varId)
        }
        case AddressibleLocalVariable2(varId, variability, reference) => {
          translateAddressibleLetAndLend(
            hinputs, hamuts2, locals1, stackHeight1, nodesByLine1, sourceExpr2, sourceExprResultLine, sourceResultPointerTypeH, let2, varId, variability, reference)
        }
      }

    val (hamuts5, locals5, stackHeightH, nodesByLine5) =
      ExpressionHammer.translateDeferreds(
        hinputs, hamuts4, localsH, stackHeight2, nodesByLineH, deferreds)

    (hamuts5, locals5, stackHeightH, nodesByLine5, borrowAccess)
  }

  private def translateAddressibleLet(
      hinputs: Hinputs,
      hamuts2: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine1: Vector[NodeH],
      sourceExprResultLine: RegisterAccessH[ReferendH],
      sourceResultPointerTypeH: ReferenceH[ReferendH],
      varId: VariableId2,
      variability: Variability,
      reference: Coord):
  (Hamuts, Locals, StackHeight, Vector[NodeH]) = {
    val (hamutsH, boxStructRefH) =
      StructHammer.makeBox(hinputs, hamuts2, variability, reference, sourceResultPointerTypeH)
    val expectedLocalBoxType = ReferenceH(Own, boxStructRefH)

    val (locals2, local) =
      locals1.addTemplarLocal(varId, stackHeight0, expectedLocalBoxType)
    val stackHeight1 = stackHeight0.oneLocalHigher()
    val (nodesByLine2, boxNode) =
      addNode(
        nodesByLine1,
        NewStructH(
          newId(nodesByLine1),
          List(sourceExprResultLine),
          expectedLocalBoxType))
    val (nodesByLineH, stackNode) =
      addNode(
        nodesByLine2,
        StackifyH(
          newId(nodesByLine2),
          RegisterAccessH(boxNode.registerId, expectedLocalBoxType),
          local,
          varId.variableName))
    val _ = stackNode // Don't need it

    (hamutsH, locals2, stackHeight1, nodesByLineH)
  }

  private def translateAddressibleLetAndLend(
      hinputs: Hinputs,
      hamuts2: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine1: Vector[NodeH],
      sourceExpr2: ReferenceExpression2,
      sourceExprResultLine: RegisterAccessH[ReferendH],
      sourceResultPointerTypeH: ReferenceH[ReferendH],
      let2: LetAndLend2,
      varId: VariableId2,
      variability: Variability,
      reference: Coord):
  (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH]) = {
    val (hamutsH, locals2, stackHeight1, nodesByLineH) =
      translateAddressibleLet(
        hinputs, hamuts2, locals1, stackHeight0, nodesByLine1, sourceExprResultLine, sourceResultPointerTypeH, varId, variability, reference)
    val (hamuts4, localsH, stackHeight2, nodesByLine4, borrowAccess, List()) =
      LoadHammer.translateAddressibleLocalLoad(
        hinputs,
        hamutsH,
        locals2,
        stackHeight1,
        nodesByLineH,
        varId,
        variability,
        sourceExpr2.resultRegister.reference,
        let2.resultRegister.reference.ownership)
    (hamuts4, localsH, stackHeight2, nodesByLine4, borrowAccess)
  }

  private def translateMundaneLet(
      hinputs: Hinputs,
      hamuts2: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine1: Vector[NodeH],
      sourceExprResultLine: RegisterAccessH[ReferendH],
      sourceResultPointerTypeH: ReferenceH[ReferendH],
      varId: VariableId2):
  (Hamuts, Locals, StackHeight, Vector[NodeH]) = {
    val (locals2, localIndex) =
      locals1.addTemplarLocal(varId, stackHeight0, sourceResultPointerTypeH)
    val stackHeight1 = stackHeight0.oneLocalHigher()
    val (nodesByLine2, stackNode) =
      addNode(
        nodesByLine1,
        StackifyH(
          newId(nodesByLine1),
          sourceExprResultLine,
          localIndex,
          varId.variableName))
    val _ = stackNode // Don't need it

    (hamuts2, locals2, stackHeight1, nodesByLine2)
  }

    private def translateMundaneLetAndLend(
        hinputs: Hinputs,
        hamuts0: Hamuts,
        locals0: Locals,
        stackHeight0: StackHeight,
        nodesByLine0: Vector[NodeH],
        sourceExpr2: ReferenceExpression2,
        sourceExprResultLine: RegisterAccessH[ReferendH],
        sourceResultPointerTypeH: ReferenceH[ReferendH],
        let2: LetAndLend2,
        varId: VariableId2):
    (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH]) = {
      val (hamuts1, locals1, stackHeight1, nodesByLine1) =
        translateMundaneLet(
          hinputs,
          hamuts0,
          locals0,
          stackHeight0,
          nodesByLine0,
          sourceExprResultLine,
          sourceResultPointerTypeH,
          varId)

    val (hamutsH, localsH, stackHeightH, nodesByLine4, borrowAccess, List()) =
      LoadHammer.translateMundaneLocalLoad(
        hinputs,
        hamuts1,
        locals1,
        stackHeight1,
        nodesByLine1,
        varId,
        sourceExpr2.resultRegister.reference,
        let2.resultRegister.reference.ownership)

    (hamutsH, localsH, stackHeightH, nodesByLine4, borrowAccess)
  }

  def translateUnlet(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      unlet2: Unlet2):
  (Hamuts, Locals, StackHeight, Vector[NodeH], RegisterAccessH[ReferendH]) = {
    val local =
      locals0.get(unlet2.variable.id) match {
        case None => {
          vfail("Unletting an unknown variable: " + unlet2.variable.id)
        }
        case Some(local) => local
      }

    unlet2.variable match {
      case ReferenceLocalVariable2(varId, _, localType2) => {
        val (hamutsH, localTypeH) =
          TypeHammer.translateReference(hinputs, hamuts0, localType2)
        val (nodesByLine1, unstackifyNode) =
          addNode(
            nodesByLine0,
            UnstackifyH(
              newId(nodesByLine0),
              local,
              localTypeH))
        val valueAccess =
          RegisterAccessH(unstackifyNode.registerId, localTypeH)
        val locals1 = locals0.markUnstackified(varId)
        (hamutsH, locals1, stackHeight0, nodesByLine1, valueAccess)
      }
      case AddressibleLocalVariable2(varId, variability, innerType2) => {
        val (hamuts1, innerTypeH) =
          TypeHammer.translateReference(hinputs, hamuts0, innerType2)
        val (hamuts2, structRefH) =
          StructHammer.makeBox(hinputs, hamuts1, variability, innerType2, innerTypeH)
        val localTypeH = ReferenceH(Own, structRefH)

        val (nodesByLine1, unstackifyBoxNode) =
          addNode(
            nodesByLine0,
            UnstackifyH(
              newId(nodesByLine0),
              local,
              localTypeH))
        val boxValueAccess =
          RegisterAccessH(unstackifyBoxNode.registerId, localTypeH)
        val locals1 = locals0.markUnstackified(varId)

        val (locals2, innerLocal) =
          locals1.addHammerLocal(stackHeight0, innerTypeH)
        val stackHeight1 = stackHeight0.oneLocalHigher()

        val (nodesByLine2, _) =
          addNode(
            nodesByLine1,
            DestructureH(
              newId(nodesByLine1),
              boxValueAccess,
              List(innerTypeH),
              Vector(innerLocal)))
        val localsH = locals2.markUnstackified(innerLocal.id)

        val (nodesByLineH, unstackifyContentsNode) =
          addNode(
            nodesByLine2,
            UnstackifyH(
              newId(nodesByLine2),
              innerLocal,
              innerTypeH))
        val contentsValueAccess =
          RegisterAccessH(unstackifyContentsNode.registerId, innerTypeH)

        (hamuts2, localsH, stackHeight1, nodesByLineH, contentsValueAccess)
      }
    }
  }

  def translateDestructure(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[NodeH],
      des2: Destructure2):
  (Hamuts, Locals, StackHeight, Vector[NodeH]) = {
    val Destructure2(sourceExpr2, structRef2, destinationReferenceLocalVariables) = des2

    val (hamuts2, locals1, stackHeight1, nodesByLine1, Some(sourceExprResultLine), sourceExprDeferreds) =
      translate(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, sourceExpr2);

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
    val (hamuts10, locals5, stackHeight5, List(), localTypes, localIndices) =
      structDef2.members.foldLeft((hamuts2, locals1, stackHeight1, destinationReferenceLocalVariables, List[ReferenceH[ReferendH]](), List[Local]()))({
        case ((hamutsH, localsH, stackHeightH, remainingDestinationReferenceLocalVariables, previousLocalTypes, previousLocalIndices), member2) => {
          member2.tyype match {
            case ReferenceMemberType2(memberRefType2) => {
              val destinationReferenceLocalVariable = remainingDestinationReferenceLocalVariables.head

              val (hamuts4, memberRefTypeH) =
                TypeHammer.translateReference(hinputs, hamutsH, memberRefType2)
              val (locals4, localIndex) =
                localsH.addTemplarLocal(
                  destinationReferenceLocalVariable.id, stackHeightH, memberRefTypeH)
              val stackHeight4 = stackHeightH.oneLocalHigher()
              (hamuts4, locals4, stackHeight4, remainingDestinationReferenceLocalVariables.tail, previousLocalTypes :+ memberRefTypeH, previousLocalIndices :+ localIndex)
            }
            // The struct might have addressibles in them, which translate to
            // borrow refs of boxes which contain things. We're moving that borrow
            // ref into a local variable. We'll then unlet the local variable, and
            // unborrow it.
            case AddressMemberType2(memberRefType2) => {
              val (hamuts4, memberRefTypeH) =
                TypeHammer.translateReference(hinputs, hamutsH, memberRefType2);
              // In the case of an addressible struct member, its variability refers to the
              // variability of the pointee variable, see StructMember2
              val (hamuts5, boxStructRefH) =
                StructHammer.makeBox(hinputs, hamuts4, member2.variability, memberRefType2, memberRefTypeH)
              // Structs only ever borrow boxes, boxes are only ever owned by the stack.
              val localBoxType = ReferenceH(Borrow, boxStructRefH)
              val (locals4, localIndex) =
                localsH.addHammerLocal(stackHeightH, localBoxType)
              val stackHeight4 = stackHeightH.oneLocalHigher()

              (hamuts5, locals4, stackHeight4, remainingDestinationReferenceLocalVariables, previousLocalTypes :+ localBoxType, previousLocalIndices :+ localIndex)
            }
          }
        }
      })

    val (nodesByLine2, stackNode) =
      addNode(
        nodesByLine1,
        DestructureH(
          newId(nodesByLine1),
          sourceExprResultLine.expectStructAccess(),
          localTypes,
          localIndices.toVector))
    val _ = stackNode // Don't need it

    val (locals6, nodesByLine6) =
      structDef2.members.zip(localTypes.zip(localIndices)).foldLeft((locals5, nodesByLine2))({
        case ((locals2, nodesByLineH), (structMember2, (localType, local))) => {
          structMember2.tyype match {
            case ReferenceMemberType2(_) => (locals2, nodesByLineH)
            case AddressMemberType2(_) => {
              // localType is the box type.
              // First, unlet it, then discard it.
              val (nodesByLine4, unstackifyNode) =
                addNode(
                  nodesByLineH,
                  UnstackifyH(
                    newId(nodesByLineH),
                    local,
                    localType))
              val unstackifiedAccess =
                RegisterAccessH(unstackifyNode.registerId, localType)
              val localsH = locals2.markUnstackified(local.id)

              val (nodesByLine5, discardNode) =
                addNode(
                  nodesByLine4,
                  DiscardH(
                    newId(nodesByLine4),
                    unstackifiedAccess))
              val _ = discardNode

              (localsH, nodesByLine5)
            }
          }
        }
      })

    val (hamuts11, locals11, stackHeight11, nodesByLine11) =
      ExpressionHammer.translateDeferreds(
        hinputs, hamuts10, locals6, stackHeight5, nodesByLine6, sourceExprDeferreds)

    (hamuts11, locals11, stackHeight11, nodesByLine11)
  }
}
