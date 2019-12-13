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
      nodesByLine0: Vector[Node3],
      let2: LetNormal2):
  (Hamuts, Locals, StackHeight, Vector[Node3]) = {
    val LetNormal2(localVariable, sourceExpr2) = let2

    val (hamuts1, locals1, stackHeight1, nodesByLine1, Some(sourceExprResultLine), deferreds) =
      translate(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, sourceExpr2);
    val (hamuts2, sourceResultPointerType3) =
      TypeHammer.translateReference(hinputs, hamuts1, sourceExpr2.resultRegister.reference)

    val (hamuts4, locals3, stackHeight2, nodesByLine3) =
      localVariable match {
        case ReferenceLocalVariable2(varId, variability, type2) => {
          translateMundaneLet(
            hinputs, hamuts2, locals1, stackHeight1, nodesByLine1, sourceExprResultLine, sourceResultPointerType3, varId)
        }
        case AddressibleLocalVariable2(varId, variability, reference) => {
          translateAddressibleLet(
            hinputs, hamuts2, locals1, stackHeight1, nodesByLine1, sourceExprResultLine, sourceResultPointerType3, varId, variability, reference)
        }
      }

    val (hamuts5, locals4, stackHeight3, nodesByLine5) =
      ExpressionHammer.translateDeferreds(
        hinputs, hamuts4, locals3, stackHeight2, nodesByLine3, deferreds)

    (hamuts5, locals4, stackHeight3, nodesByLine5)
  }

  def translateLetAndLend(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      let2: LetAndLend2):
  (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3]) = {
    val LetAndLend2(localVariable, sourceExpr2) = let2

    val (hamuts1, locals1, stackHeight1, nodesByLine1, Some(sourceExprResultLine), deferreds) =
      translate(hinputs, hamuts0, locals0, stackHeight0, nodesByLine0, sourceExpr2);
    val (hamuts2, sourceResultPointerType3) =
      TypeHammer.translateReference(hinputs, hamuts1, sourceExpr2.resultRegister.reference)

    val (hamuts4, locals3, stackHeight2, nodesByLine3, borrowAccess) =
      localVariable match {
        case ReferenceLocalVariable2(varId, variability, type2) => {
          translateMundaneLetAndLend(
            hinputs, hamuts2, locals1, stackHeight1, nodesByLine1, sourceExpr2, sourceExprResultLine, sourceResultPointerType3, let2, varId)
        }
        case AddressibleLocalVariable2(varId, variability, reference) => {
          translateAddressibleLetAndLend(
            hinputs, hamuts2, locals1, stackHeight1, nodesByLine1, sourceExpr2, sourceExprResultLine, sourceResultPointerType3, let2, varId, variability, reference)
        }
      }

    val (hamuts5, locals5, stackHeight3, nodesByLine5) =
      ExpressionHammer.translateDeferreds(
        hinputs, hamuts4, locals3, stackHeight2, nodesByLine3, deferreds)

    (hamuts5, locals5, stackHeight3, nodesByLine5, borrowAccess)
  }

  private def translateAddressibleLet(
      hinputs: Hinputs,
      hamuts2: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine1: Vector[Node3],
      sourceExprResultLine: RegisterAccess3[Referend3],
      sourceResultPointerType3: Reference3[Referend3],
      varId: VariableId2,
      variability: Variability,
      reference: Coord):
  (Hamuts, Locals, StackHeight, Vector[Node3]) = {
    val (hamuts3, boxStructRef3) =
      StructHammer.makeBox(hinputs, hamuts2, variability, reference, sourceResultPointerType3)
    val expectedLocalBoxType = Reference3(Own, boxStructRef3)

    val (locals2, local) =
      locals1.addTemplarLocal(varId, stackHeight0, expectedLocalBoxType)
    val stackHeight1 = stackHeight0.oneLocalHigher()
    val (nodesByLine2, boxNode) =
      addNode(
        nodesByLine1,
        NewStruct3(
          newId(nodesByLine1),
          List(sourceExprResultLine),
          expectedLocalBoxType))
    val (nodesByLine3, stackNode) =
      addNode(
        nodesByLine2,
        Stackify3(
          newId(nodesByLine2),
          RegisterAccess3(boxNode.registerId, expectedLocalBoxType),
          local,
          varId.variableName))
    val _ = stackNode // Don't need it

    (hamuts3, locals2, stackHeight1, nodesByLine3)
  }

  private def translateAddressibleLetAndLend(
      hinputs: Hinputs,
      hamuts2: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine1: Vector[Node3],
      sourceExpr2: ReferenceExpression2,
      sourceExprResultLine: RegisterAccess3[Referend3],
      sourceResultPointerType3: Reference3[Referend3],
      let2: LetAndLend2,
      varId: VariableId2,
      variability: Variability,
      reference: Coord):
  (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3]) = {
    val (hamuts3, locals2, stackHeight1, nodesByLine3) =
      translateAddressibleLet(
        hinputs, hamuts2, locals1, stackHeight0, nodesByLine1, sourceExprResultLine, sourceResultPointerType3, varId, variability, reference)
    val (hamuts4, locals3, stackHeight2, nodesByLine4, borrowAccess, List()) =
      LoadHammer.translateAddressibleLocalLoad(
        hinputs,
        hamuts3,
        locals2,
        stackHeight1,
        nodesByLine3,
        varId,
        variability,
        sourceExpr2.resultRegister.reference,
        let2.resultRegister.reference.ownership)
    (hamuts4, locals3, stackHeight2, nodesByLine4, borrowAccess)
  }

  private def translateMundaneLet(
      hinputs: Hinputs,
      hamuts2: Hamuts,
      locals1: Locals,
      stackHeight0: StackHeight,
      nodesByLine1: Vector[Node3],
      sourceExprResultLine: RegisterAccess3[Referend3],
      sourceResultPointerType3: Reference3[Referend3],
      varId: VariableId2):
  (Hamuts, Locals, StackHeight, Vector[Node3]) = {
    val (locals2, localIndex) =
      locals1.addTemplarLocal(varId, stackHeight0, sourceResultPointerType3)
    val stackHeight1 = stackHeight0.oneLocalHigher()
    val (nodesByLine2, stackNode) =
      addNode(
        nodesByLine1,
        Stackify3(
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
        nodesByLine0: Vector[Node3],
        sourceExpr2: ReferenceExpression2,
        sourceExprResultLine: RegisterAccess3[Referend3],
        sourceResultPointerType3: Reference3[Referend3],
        let2: LetAndLend2,
        varId: VariableId2):
    (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3]) = {
      val (hamuts1, locals1, stackHeight1, nodesByLine1) =
        translateMundaneLet(
          hinputs,
          hamuts0,
          locals0,
          stackHeight0,
          nodesByLine0,
          sourceExprResultLine,
          sourceResultPointerType3,
          varId)

    val (hamuts3, locals3, stackHeight3, nodesByLine4, borrowAccess, List()) =
      LoadHammer.translateMundaneLocalLoad(
        hinputs,
        hamuts1,
        locals1,
        stackHeight1,
        nodesByLine1,
        varId,
        sourceExpr2.resultRegister.reference,
        let2.resultRegister.reference.ownership)

    (hamuts3, locals3, stackHeight3, nodesByLine4, borrowAccess)
  }

  def translateUnlet(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      unlet2: Unlet2):
  (Hamuts, Locals, StackHeight, Vector[Node3], RegisterAccess3[Referend3]) = {
    val local =
      locals0.get(unlet2.variable.id) match {
        case None => {
          vfail("Unletting an unknown variable: " + unlet2.variable.id)
        }
        case Some(local) => local
      }

    unlet2.variable match {
      case ReferenceLocalVariable2(varId, _, localType2) => {
        val (hamuts3, localType3) =
          TypeHammer.translateReference(hinputs, hamuts0, localType2)
        val (nodesByLine1, unstackifyNode) =
          addNode(
            nodesByLine0,
            Unstackify3(
              newId(nodesByLine0),
              local,
              localType3))
        val valueAccess =
          RegisterAccess3(unstackifyNode.registerId, localType3)
        val locals1 = locals0.markUnstackified(varId)
        (hamuts3, locals1, stackHeight0, nodesByLine1, valueAccess)
      }
      case AddressibleLocalVariable2(varId, variability, innerType2) => {
        val (hamuts1, innerType3) =
          TypeHammer.translateReference(hinputs, hamuts0, innerType2)
        val (hamuts2, structRef3) =
          StructHammer.makeBox(hinputs, hamuts1, variability, innerType2, innerType3)
        val localType3 = Reference3(Own, structRef3)

        val (nodesByLine1, unstackifyBoxNode) =
          addNode(
            nodesByLine0,
            Unstackify3(
              newId(nodesByLine0),
              local,
              localType3))
        val boxValueAccess =
          RegisterAccess3(unstackifyBoxNode.registerId, localType3)
        val locals1 = locals0.markUnstackified(varId)

        val (locals2, innerLocal) =
          locals1.addHammerLocal(stackHeight0, innerType3)
        val stackHeight1 = stackHeight0.oneLocalHigher()

        val (nodesByLine2, _) =
          addNode(
            nodesByLine1,
            Destructure3(
              newId(nodesByLine1),
              boxValueAccess,
              List(innerType3),
              Vector(innerLocal)))
        val locals3 = locals2.markUnstackified(innerLocal.id)

        val (nodesByLine3, unstackifyContentsNode) =
          addNode(
            nodesByLine2,
            Unstackify3(
              newId(nodesByLine2),
              innerLocal,
              innerType3))
        val contentsValueAccess =
          RegisterAccess3(unstackifyContentsNode.registerId, innerType3)

        (hamuts2, locals3, stackHeight1, nodesByLine3, contentsValueAccess)
      }
    }
  }

  def translateDestructure(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      locals0: Locals,
      stackHeight0: StackHeight,
      nodesByLine0: Vector[Node3],
      des2: Destructure2):
  (Hamuts, Locals, StackHeight, Vector[Node3]) = {
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
      structDef2.members.foldLeft((hamuts2, locals1, stackHeight1, destinationReferenceLocalVariables, List[Reference3[Referend3]](), List[Local]()))({
        case ((hamuts3, locals3, stackHeight3, remainingDestinationReferenceLocalVariables, previousLocalTypes, previousLocalIndices), member2) => {
          member2.tyype match {
            case ReferenceMemberType2(memberRefType2) => {
              val destinationReferenceLocalVariable = remainingDestinationReferenceLocalVariables.head

              val (hamuts4, memberRefType3) =
                TypeHammer.translateReference(hinputs, hamuts3, memberRefType2)
              val (locals4, localIndex) =
                locals3.addTemplarLocal(
                  destinationReferenceLocalVariable.id, stackHeight3, memberRefType3)
              val stackHeight4 = stackHeight3.oneLocalHigher()
              (hamuts4, locals4, stackHeight4, remainingDestinationReferenceLocalVariables.tail, previousLocalTypes :+ memberRefType3, previousLocalIndices :+ localIndex)
            }
            // The struct might have addressibles in them, which translate to
            // borrow refs of boxes which contain things. We're moving that borrow
            // ref into a local variable. We'll then unlet the local variable, and
            // unborrow it.
            case AddressMemberType2(memberRefType2) => {
              val (hamuts4, memberRefType3) =
                TypeHammer.translateReference(hinputs, hamuts3, memberRefType2);
              // In the case of an addressible struct member, its variability refers to the
              // variability of the pointee variable, see StructMember2
              val (hamuts5, boxStructRef3) =
                StructHammer.makeBox(hinputs, hamuts4, member2.variability, memberRefType2, memberRefType3)
              // Structs only ever borrow boxes, boxes are only ever owned by the stack.
              val localBoxType = Reference3(Borrow, boxStructRef3)
              val (locals4, localIndex) =
                locals3.addHammerLocal(stackHeight3, localBoxType)
              val stackHeight4 = stackHeight3.oneLocalHigher()

              (hamuts5, locals4, stackHeight4, remainingDestinationReferenceLocalVariables, previousLocalTypes :+ localBoxType, previousLocalIndices :+ localIndex)
            }
          }
        }
      })

    val (nodesByLine2, stackNode) =
      addNode(
        nodesByLine1,
        Destructure3(
          newId(nodesByLine1),
          sourceExprResultLine.expectStructAccess(),
          localTypes,
          localIndices.toVector))
    val _ = stackNode // Don't need it

    val (locals6, nodesByLine6) =
      structDef2.members.zip(localTypes.zip(localIndices)).foldLeft((locals5, nodesByLine2))({
        case ((locals2, nodesByLine3), (structMember2, (localType, local))) => {
          structMember2.tyype match {
            case ReferenceMemberType2(_) => (locals2, nodesByLine3)
            case AddressMemberType2(_) => {
              // localType is the box type.
              // First, unlet it, then discard it.
              val (nodesByLine4, unstackifyNode) =
                addNode(
                  nodesByLine3,
                  Unstackify3(
                    newId(nodesByLine3),
                    local,
                    localType))
              val unstackifiedAccess =
                RegisterAccess3(unstackifyNode.registerId, localType)
              val locals3 = locals2.markUnstackified(local.id)

              val (nodesByLine5, discardNode) =
                addNode(
                  nodesByLine4,
                  Discard3(
                    newId(nodesByLine4),
                    unstackifiedAccess))
              val _ = discardNode

              (locals3, nodesByLine5)
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
