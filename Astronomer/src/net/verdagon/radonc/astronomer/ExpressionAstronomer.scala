package net.verdagon.radonc.astronomer

import net.verdagon.radonc.astronomer.ruletyper.{RuleTyperSolveFailure, RuleTyperSolveSuccess}
import net.verdagon.radonc.scout._
import net.verdagon.radonc.vfail

import scala.collection.immutable.List

object ExpressionAstronomer {
  def translateBlock(env: Environment, astrouts: AstroutsBox, blockS: BlockSE): BlockAE = {
    val BlockSE(locals, exprsS) = blockS
    val exprsA = exprsS.map(translateExpression(env, astrouts, _))
    BlockAE(locals, exprsA)
  }

  def translateExpression(env: Environment, astrouts: AstroutsBox, iexprS: IExpressionSE): IExpressionAE = {
    iexprS match {
      case LetSE(patternId, rules, allRunes, pattern, expr) => {
        val (conclusions, rulesA) =
          Astronomer.makeRuleTyper().solve(astrouts, env, rules, List(pattern), Some(allRunes)) match {
            case (_, rtsf @ RuleTyperSolveFailure(_, _, _)) => vfail(rtsf.toString)
            case (c, RuleTyperSolveSuccess(r)) => (c, r)
          }
        val exprA = translateExpression(env, astrouts, expr)

        LetAE(patternId, rulesA, conclusions.typeByRune, pattern, exprA)
      }
      case IfSE(conditionS, thenBodyS, elseBodyS) => {
        val conditionA = translateBlock(env, astrouts, conditionS)
        val thenBodyA = translateBlock(env, astrouts, thenBodyS)
        val elseBodyA = translateBlock(env, astrouts, elseBodyS)
        IfAE(conditionA, thenBodyA, elseBodyA)
      }
      case WhileSE(conditionS, bodyS) => {
        val conditionA = translateBlock(env, astrouts, conditionS)
        val bodyA = translateBlock(env, astrouts, bodyS)
        WhileAE(conditionA, bodyA)
      }
      case ExprMutateSE(mutateeS, exprS) => {
        val conditionA = translateExpression(env, astrouts, mutateeS)
        val bodyA = translateExpression(env, astrouts, exprS)
        ExprMutateAE(conditionA, bodyA)
      }
      case GlobalMutateSE(name, exprS) => {
        val exprA = translateExpression(env, astrouts, exprS)
        GlobalMutateAE(name, exprA)
      }
      case LocalMutateSE(nameS, exprS) => {
        val exprA = translateExpression(env, astrouts, exprS)
        LocalMutateAE(nameS, exprA)
      }
      case ExpressionLendSE(innerExprS) => {
        val innerExprA = translateExpression(env, astrouts, innerExprS)
        ExpressionLendAE(innerExprA)
      }
      case ReturnSE(innerExprS) => {
        val innerExprA = translateExpression(env, astrouts, innerExprS)
        (ReturnAE(innerExprA))
      }
      case blockS @ BlockSE(_, _) => translateBlock(env, astrouts, blockS)
      case ArgLookupSE(index) => (ArgLookupAE(index))
      case CheckRefCountSE(refExprS, category, numExprS) => {
        val refExprA = translateExpression(env, astrouts, refExprS)
        val numExprA = translateExpression(env, astrouts, numExprS)
        (CheckRefCountAE(refExprA, category, numExprA))
      }
      case RepeaterBlockSE(exprS) => {
        val exprA = translateExpression(env, astrouts, exprS)
        (RepeaterBlockAE(exprA))
      }
      case RepeaterBlockIteratorSE(exprS) => {
        val exprA = translateExpression(env, astrouts, exprS)
        (RepeaterBlockIteratorAE(exprA))
      }
      case packS @ PackSE(_) => translatePack(astrouts, env, packS)
      case VoidSE() => VoidAE()
      case SequenceESE(elementsS) => {
        val elementsA = elementsS.map(translateExpression(env, astrouts, _))
        SequenceEAE(elementsA)
      }
      case RepeaterPackSE(exprS) => {
        val elementsA = translateExpression(env, astrouts, exprS)
        RepeaterPackAE(elementsA)
      }
      case RepeaterPackIteratorSE(exprS) => {
        val exprA = translateExpression(env, astrouts, exprS)
        RepeaterPackIteratorAE(exprA)
      }
      case IntLiteralSE(value) => IntLiteralAE(value)
      case BoolLiteralSE(value) => BoolLiteralAE(value)
      case StrLiteralSE(value) => StrLiteralAE(value)
      case FloatLiteralSE(value) => FloatLiteralAE(value)
      case FunctionSE(functionS) => {
        val functionA = Astronomer.translateFunction(astrouts, env, functionS)
        FunctionAE(functionA)
      }
      case DotSE(leftS, member, borrowContainer) => {
        val leftA = translateExpression(env, astrouts, leftS)
        DotAE(leftA, member, borrowContainer)
      }
      case DotCallSE(leftS, indexExprS) => {
        val leftA = translateExpression(env, astrouts, leftS)
        val indexExprA = translateExpression(env, astrouts, indexExprS)
        DotCallAE(leftA, indexExprA)
      }
      case FunctionCallSE(callableExprS, argsPackExprS) => {
        val callableExprA = translateExpression(env, astrouts, callableExprS)
        val argsPackExprA = translatePack(astrouts, env, argsPackExprS)
        FunctionCallAE(callableExprA, argsPackExprA)
      }
      case TemplateSpecifiedLookupSE(name, templateArgsS) => {
        // We don't translate the templexes, we can't until we know what the template expects.
        TemplateSpecifiedLookupAE(name, templateArgsS)
      }
      case LocalLoadSE(name, borrow) => LocalLoadAE(name, borrow)
      case GlobalLoadSE(name) => GlobalLoadAE(name)
      case UnletSE(name) => UnletAE(name)
    }
  }

  private def translatePack(astrouts: AstroutsBox, env: Environment, packS: PackSE):
  PackAE = {
    val elementsA = packS.elements.map(translateExpression(env, astrouts, _))
    PackAE(elementsA)
  }
}
