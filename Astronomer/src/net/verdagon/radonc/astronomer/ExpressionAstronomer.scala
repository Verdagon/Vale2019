package net.verdagon.radonc.astronomer

import net.verdagon.radonc.astronomer.ruletyper.{RuleTyperSolveFailure, RuleTyperSolveSuccess}
import net.verdagon.radonc.scout._
import net.verdagon.radonc.vfail

import scala.collection.immutable.List

object ExpressionAstronomer {
  def translateBlock(env: Environment, astrouts0: Astrouts, blockS: BlockSE): (Astrouts, BlockAE) = {
    val BlockSE(locals, exprsS) = blockS

    val (astrouts10, exprsA) = translateExpressions(env, astrouts0, exprsS)

    (astrouts10, BlockAE(locals, exprsA))
  }

  def translateExpressions(env: Environment, astrouts0: Astrouts, exprsS: List[IExpressionSE]): (Astrouts, List[IExpressionAE]) = {
    exprsS match {
      case Nil => (astrouts0, Nil)
      case headS :: tailS => {
        val (astrouts10, headA) = translateExpression(env, astrouts0, headS)
        val (astrouts20, tailA) = translateExpressions(env, astrouts10, tailS)
        (astrouts20, headA :: tailA)
      }
    }
  }

  def translateExpression(env: Environment, astrouts0: Astrouts, iexprS: IExpressionSE): (Astrouts, IExpressionAE) = {
    iexprS match {
      case LetSE(patternId, rules, allRunes, pattern, expr) => {
        val (astrouts10, conclusions, rulesA) =
          Astronomer.makeRuleTyper().solve(astrouts0, env, rules, List(pattern), Some(allRunes)) match {
            case (_, rtsf @ RuleTyperSolveFailure(_, _, _)) => vfail(rtsf.toString)
            case (astrouts1, RuleTyperSolveSuccess(c, r)) => (astrouts1, c, r)
          }
        val (astrouts20, exprA) = translateExpression(env, astrouts10, expr)

        (astrouts20, LetAE(patternId, rulesA, conclusions.typeByRune, pattern, exprA))
      }
      case IfSE(conditionS, thenBodyS, elseBodyS) => {
        val (astrouts10, conditionA) = translateBlock(env, astrouts0, conditionS)
        val (astrouts20, thenBodyA) = translateBlock(env, astrouts10, thenBodyS)
        val (astrouts30, elseBodyA) = translateBlock(env, astrouts20, elseBodyS)
        (astrouts30, IfAE(conditionA, thenBodyA, elseBodyA))
      }
      case WhileSE(conditionS, bodyS) => {
        val (astrouts10, conditionA) = translateBlock(env, astrouts0, conditionS)
        val (astrouts20, bodyA) = translateBlock(env, astrouts10, bodyS)
        (astrouts20, WhileAE(conditionA, bodyA))
      }
      case ExprMutateSE(mutateeS, exprS) => {
        val (astrouts10, conditionA) = translateExpression(env, astrouts0, mutateeS)
        val (astrouts20, bodyA) = translateExpression(env, astrouts10, exprS)
        (astrouts20, ExprMutateAE(conditionA, bodyA))
      }
      case GlobalMutateSE(name, exprS) => {
        val (astrouts10, exprA) = translateExpression(env, astrouts0, exprS)
        (astrouts10, GlobalMutateAE(name, exprA))
      }
      case LocalMutateSE(nameS, exprS) => {
        val (astrouts10, exprA) = translateExpression(env, astrouts0, exprS)
        (astrouts10, LocalMutateAE(nameS, exprA))
      }
      case ExpressionLendSE(innerExprS) => {
        val (astrouts10, innerExprA) = translateExpression(env, astrouts0, innerExprS)
        (astrouts10, ExpressionLendAE(innerExprA))
      }
      case ReturnSE(innerExprS) => {
        val (astrouts10, innerExprA) = translateExpression(env, astrouts0, innerExprS)
        (astrouts10, ReturnAE(innerExprA))
      }
      case blockS @ BlockSE(_, _) => translateBlock(env, astrouts0, blockS)
      case ArgLookupSE(index) => (astrouts0, ArgLookupAE(index))
      case CheckRefCountSE(refExprS, category, numExprS) => {
        val (astrouts10, refExprA) = translateExpression(env, astrouts0, refExprS)
        val (astrouts20, numExprA) = translateExpression(env, astrouts10, numExprS)
        (astrouts20, CheckRefCountAE(refExprA, category, numExprA))
      }
      case RepeaterBlockSE(exprS) => {
        val (astrouts10, exprA) = translateExpression(env, astrouts0, exprS)
        (astrouts10, RepeaterBlockAE(exprA))
      }
      case RepeaterBlockIteratorSE(exprS) => {
        val (astrouts10, exprA) = translateExpression(env, astrouts0, exprS)
        (astrouts10, RepeaterBlockIteratorAE(exprA))
      }
      case packS @ PackSE(_) => translatePack(astrouts0, env, packS)
      case VoidSE() => (astrouts0, VoidAE())
      case SequenceESE(elementsS) => {
        val (astrouts10, elementsA) = translateExpressions(env, astrouts0, elementsS)
        (astrouts10, SequenceEAE(elementsA))
      }
      case RepeaterPackSE(exprS) => {
        val (astrouts10, elementsA) = translateExpression(env, astrouts0, exprS)
        (astrouts10, RepeaterPackAE(elementsA))
      }
      case RepeaterPackIteratorSE(exprS) => {
        val (astrouts10, exprA) = translateExpression(env, astrouts0, exprS)
        (astrouts10, RepeaterPackIteratorAE(exprA))
      }
      case IntLiteralSE(value) => (astrouts0, IntLiteralAE(value))
      case BoolLiteralSE(value) => (astrouts0, BoolLiteralAE(value))
      case StrLiteralSE(value) => (astrouts0, StrLiteralAE(value))
      case FloatLiteralSE(value) => (astrouts0, FloatLiteralAE(value))
      case FunctionSE(functionS) => {
        val (astrouts10, functionA) = Astronomer.translateFunction(astrouts0, env, functionS)
        (astrouts10, FunctionAE(functionA))
      }
      case DotSE(leftS, member, borrowContainer) => {
        val (astrouts10, leftA) = translateExpression(env, astrouts0, leftS)
        (astrouts10, DotAE(leftA, member, borrowContainer))
      }
      case DotCallSE(leftS, indexExprS) => {
        val (astrouts10, leftA) = translateExpression(env, astrouts0, leftS)
        val (astrouts20, indexExprA) = translateExpression(env, astrouts10, indexExprS)
        (astrouts20, DotCallAE(leftA, indexExprA))
      }
      case FunctionCallSE(callableExprS, argsPackExprS) => {
        val (astrouts10, callableExprA) = translateExpression(env, astrouts0, callableExprS)
        val (astrouts20, argsPackExprA) = translatePack(astrouts10, env, argsPackExprS)
        (astrouts20, FunctionCallAE(callableExprA, argsPackExprA))
      }
      case TemplateSpecifiedLookupSE(name, templateArgsS) => {
        // We don't translate the templexes, we can't until we
        (astrouts0, TemplateSpecifiedLookupAE(name, templateArgsS))
      }
      case LocalLoadSE(name, borrow) => (astrouts0, LocalLoadAE(name, borrow))
      case GlobalLoadSE(name) => (astrouts0, GlobalLoadAE(name))
      case UnletSE(name) => (astrouts0, UnletAE(name))
    }
  }

  private def translatePack(astrouts0: Astrouts, env: Environment, packS: PackSE):
  (Astrouts, PackAE) = {
    val (astrouts10, elementsA) = translateExpressions(env, astrouts0, packS.elements)
    (astrouts10, PackAE(elementsA))
  }
}
