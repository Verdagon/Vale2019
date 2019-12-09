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
    parentFate: FunctionEnvironment,
    temputs: TemputsBox,
    block1: BlockAE):
  (FunctionEnvironment, Block2, Set[Coord]) = {

    val newCounter = 1
    val fate0 = FunctionEnvironment(parentFate, parentFate.fullName, parentFate.function, Map(), parentFate.maybeReturnType, Set(), newCounter, List(), Set())

    val fate1 = fate0.addScoutedLocals(block1.locals)

    val (fate2, unresultifiedUndestructedExpressions, returnsFromExprs) =
      evaluateBlockStatements(temputs, fate1, block1.exprs);

    val (fate6, undestructedExpressions, maybeResultLocalVariable) =
      resultifyExpressions(fate2, unresultifiedUndestructedExpressions)

    val (fate7, expressions) =
      unletUnmovedVariablesIntroducedSince(temputs, fate0, fate6, maybeResultLocalVariable, undestructedExpressions)

    val (fate9, expressionsWithResult) =
      maybeAddUnlet(fate7, expressions, maybeResultLocalVariable)

    val block2 = Block2(expressionsWithResult)

    // We don't just use the old parentFate because this one might have had moveds added to it.
    val newParentFate @ FunctionEnvironment(_, _, _, _, _, _, _, _, _) = fate9.parentEnv

    (newParentFate, block2, returnsFromExprs)
  }

  def maybeAddUnlet(
    fate0: FunctionEnvironment,
    exprs: List[ReferenceExpression2],
    maybeVar: Option[ILocalVariable2]):
  (FunctionEnvironment, List[ReferenceExpression2]) = {
    maybeVar match {
      case None => (fate0, exprs)
      case Some(resultLocalVariable) => {
        val (fate8, getResultExpr) =
          ExpressionTemplar.unletLocal(fate0, resultLocalVariable)
        (fate8, exprs :+ getResultExpr)
      }
    }
  }

  def unletUnmovedVariablesIntroducedSince(
    temputs: TemputsBox,
    sinceFate: FunctionEnvironment,
    currentFate: FunctionEnvironment,
    maybeExcludeVar: Option[ILocalVariable2],
    exprs: List[ReferenceExpression2]):
  (FunctionEnvironment,List[ReferenceExpression2]) = {
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
    val (newFate, destructExprs) =
      unletAll(temputs, currentFate, reversedVariablesToDestruct)

    (newFate, exprs ++ destructExprs)
  }

  // Makes the last expression stored in a variable, unless there are none, or it returns void.
  def resultifyExpressions(fate0: FunctionEnvironment, undecayedExprs2: List[ReferenceExpression2]):
  (FunctionEnvironment, List[ReferenceExpression2], Option[ReferenceLocalVariable2]) = {
    undecayedExprs2.lastOption match {
      case None => (fate0, undecayedExprs2, None)
      case Some(lastExpr) => {
        if (lastExpr.resultRegister.referend == Void2()) {
          (fate0, undecayedExprs2, None)
        } else {
          val (fate4, resultVarNum) = fate0.nextVarCounter()
          val resultVarId = VariableId2(fate0.function.lambdaNumber, "__blockresult_" + resultVarNum)
          val resultVariable = ReferenceLocalVariable2(resultVarId, Final, lastExpr.resultRegister.reference)
          val resultLet = LetNormal2(resultVariable, lastExpr)
          val fate5 = fate4.addVariable(resultVariable)
          (fate5, undecayedExprs2.init :+ resultLet, Some(resultVariable))
        }
      }
    }
  }

  def evaluateBlockStatements(
    temputs: TemputsBox,
    fate0: FunctionEnvironment,
    expr1: List[IExpressionAE]):
  (FunctionEnvironment, List[ReferenceExpression2], Set[Coord]) = {
    expr1 match {
      case Nil => (fate0, List(), Set())
      case first1 :: rest1 => {
        val (fate1, undestructedFirstExpr2, returnsFromFirst) =
          ExpressionTemplar.evaluateAndCoerceToReferenceExpression(
            temputs, fate0, first1);

        val (fate2, destructedFirstExpr2) =
          if (rest1.isEmpty) {
            // This is the last expression, which means it's getting returned,
            // so don't destruct it.
            (fate1, undestructedFirstExpr2) // Do nothing
          } else {
            val (fate2, destructedFirstExpr2) =
              DestructorTemplar.drop(fate1, temputs, undestructedFirstExpr2)
            (fate2, destructedFirstExpr2)
          }

        val (fate4, restExprs2, returnsFromRest) =
          evaluateBlockStatements(temputs, fate2, rest1)

        (fate4, destructedFirstExpr2 +: restExprs2, returnsFromFirst ++ returnsFromRest)
      }
    }
  }

  def unletAll(
    temputs: TemputsBox,
    fate0: FunctionEnvironment,
    variables: List[ILocalVariable2]):
  (FunctionEnvironment, List[ReferenceExpression2]) = {
    variables match {
      case Nil => (fate0, List())
      case head :: tail => {
        val (fate1, unlet) = ExpressionTemplar.unletLocal(fate0, head)
        val (fate2, maybeHeadExpr2) =
          DestructorTemplar.drop(fate1, temputs, unlet)
        val (fate3, tailExprs2) =
          unletAll(temputs, fate2, tail)
        (fate3, maybeHeadExpr2 :: tailExprs2)
      }
    }
  }
}
