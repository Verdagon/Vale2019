package net.verdagon.radonc.templar

import net.verdagon.radonc.astronomer.{BlockAE, IExpressionAE}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.parser.FinalP
import net.verdagon.radonc.templar.env._
import net.verdagon.radonc.templar.function.DestructorTemplar
import net.verdagon.radonc.{vassert, vcurious}

object BlockTemplar {
  // This is NOT USED FOR EVERY BLOCK!
  // This is just for the simplest kind of block.
  // This can serve as an example for how we can use together all the tools provided by BlockTemplar.
  def evaluateBlock(
    parentFate: FunctionEnvironmentBox,
    temputs: TemputsBox,
    block1: BlockAE):
  (Block2, Set[Coord]) = {
    val newCounter = 1
    val fate =
      FunctionEnvironmentBox(
        FunctionEnvironment(
          parentFate.snapshot, parentFate.fullName, parentFate.function, Map(), parentFate.maybeReturnType, Set(), newCounter, List(), Set()))
    val startingFate = fate.snapshot


      fate.addScoutedLocals(block1.locals)

    val (unresultifiedUndestructedExpressions, returnsFromExprs) =
      evaluateBlockStatements(temputs, fate, block1.exprs);

    val (undestructedExpressions, maybeResultLocalVariable) =
      resultifyExpressions(fate, unresultifiedUndestructedExpressions)

    val (expressions) =
      unletUnmovedVariablesIntroducedSince(temputs, startingFate, fate, maybeResultLocalVariable, undestructedExpressions)

    val (expressionsWithResult) =
      maybeAddUnlet(fate, expressions, maybeResultLocalVariable)

    val block2 = Block2(expressionsWithResult)

    // We don't just use the old parentFate because this one might have had moveds added to it.
    val newParentFate @ FunctionEnvironment(_, _, _, _, _, _, _, _, _) = fate.parentEnv
    parentFate.functionEnvironment = newParentFate

    (block2, returnsFromExprs)
  }

  def maybeAddUnlet(
    fate: FunctionEnvironmentBox,
    exprs: List[ReferenceExpression2],
    maybeVar: Option[ILocalVariable2]):
  (List[ReferenceExpression2]) = {
    maybeVar match {
      case None => (exprs)
      case Some(resultLocalVariable) => {
        val (getResultExpr) =
          ExpressionTemplar.unletLocal(fate, resultLocalVariable)
        (exprs :+ getResultExpr)
      }
    }
  }

  def unletUnmovedVariablesIntroducedSince(
    temputs: TemputsBox,
    sinceFate: FunctionEnvironment,
    currentFate: FunctionEnvironmentBox,
    maybeExcludeVar: Option[ILocalVariable2],
    exprs: List[ReferenceExpression2]):
  List[ReferenceExpression2] = {
    val localsAsOfThen =
      sinceFate.variables.collect({
        case x @ ReferenceLocalVariable2(_, _, _) => x
        case x @ AddressibleLocalVariable2(_, _, _) => x
      })
    val localsAsOfNow =
      currentFate.variables.collect({
        case x @ ReferenceLocalVariable2(_, _, _) => x
        case x @ AddressibleLocalVariable2(_, _, _) => x
      })

    vassert(localsAsOfNow.startsWith(localsAsOfThen))
    val localsDeclaredSinceThen = localsAsOfNow.slice(localsAsOfThen.size, localsAsOfNow.size)
    vassert(localsDeclaredSinceThen.size == localsAsOfNow.size - localsAsOfThen.size)

    val unmovedLocalsDeclaredSinceThen =
      localsDeclaredSinceThen.filter(x => !currentFate.moveds.contains(x.id))

    val unmovedLocalsDeclaredSinceThenExceptExcluded =
      unmovedLocalsDeclaredSinceThen.filter(x => Some(x) != maybeExcludeVar)

    val unreversedVariablesToDestruct = unmovedLocalsDeclaredSinceThenExceptExcluded

    val reversedVariablesToDestruct = unreversedVariablesToDestruct.reverse
    // Dealiasing should be done by hammer. But destructors are done here
    val destructExprs =
      unletAll(temputs, currentFate, reversedVariablesToDestruct)

    exprs ++ destructExprs
  }

  // Makes the last expression stored in a variable, unless there are none, or it returns void.
  def resultifyExpressions(fate: FunctionEnvironmentBox, undecayedExprs2: List[ReferenceExpression2]):
  (List[ReferenceExpression2], Option[ReferenceLocalVariable2]) = {
    undecayedExprs2.lastOption match {
      case None => (undecayedExprs2, None)
      case Some(lastExpr) => {
        if (lastExpr.resultRegister.referend == Void2()) {
          (undecayedExprs2, None)
        } else {
          val (resultVarNum) = fate.nextVarCounter()
          val resultVarId = VariableId2(fate.function.lambdaNumber, "__blockresult_" + resultVarNum)
          val resultVariable = ReferenceLocalVariable2(resultVarId, Final, lastExpr.resultRegister.reference)
          val resultLet = LetNormal2(resultVariable, lastExpr)
          fate.addVariable(resultVariable)
          (undecayedExprs2.init :+ resultLet, Some(resultVariable))
        }
      }
    }
  }

  def evaluateBlockStatements(
    temputs: TemputsBox,
    fate: FunctionEnvironmentBox,
    expr1: List[IExpressionAE]):
  (List[ReferenceExpression2], Set[Coord]) = {
    expr1 match {
      case Nil => (List(), Set())
      case first1 :: rest1 => {
        val (undestructedFirstExpr2, returnsFromFirst) =
          ExpressionTemplar.evaluateAndCoerceToReferenceExpression(
            temputs, fate, first1);

        val (destructedFirstExpr2) =
          if (rest1.isEmpty) {
            // This is the last expression, which means it's getting returned,
            // so don't destruct it.
            (undestructedFirstExpr2) // Do nothing
          } else {
            val (destructedFirstExpr2) =
              DestructorTemplar.drop(fate, temputs, undestructedFirstExpr2)
            (destructedFirstExpr2)
          }

        val (restExprs2, returnsFromRest) =
          evaluateBlockStatements(temputs, fate, rest1)

        (destructedFirstExpr2 +: restExprs2, returnsFromFirst ++ returnsFromRest)
      }
    }
  }

  def unletAll(
    temputs: TemputsBox,
    fate: FunctionEnvironmentBox,
    variables: List[ILocalVariable2]):
  (List[ReferenceExpression2]) = {
    variables match {
      case Nil => (List())
      case head :: tail => {
        val (unlet) = ExpressionTemplar.unletLocal(fate, head)
        val (maybeHeadExpr2) =
          DestructorTemplar.drop(fate, temputs, unlet)
        val (tailExprs2) =
          unletAll(temputs, fate, tail)
        (maybeHeadExpr2 :: tailExprs2)
      }
    }
  }
}
