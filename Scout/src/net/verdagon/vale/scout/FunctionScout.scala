package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.ExpressionScout.NormalResult
import net.verdagon.vale.scout.Scout.noDeclarations
import net.verdagon.vale.scout.patterns._
import net.verdagon.vale.scout.predictor.Conclusions
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.scout.templatepredictor.PredictorEvaluator
import net.verdagon.vale._

import scala.collection.immutable.{List, Range}

//// Fate is short for Function State. It tracks how many magic params have been used.
//// This is similar to StackFrame, which tracks all the locals that have been declared.
//
//// Maybe we should combine them?
//// As of this writing, we use fate to keep track of magic params, and StackFrame at the
//// block level receives (via return type) declarations from the individual expressions
//// to accumulate them itself.
//case class ScoutFate(private val magicParams: Set[MagicParamNameS]) {
//  def addMagicParam(magicParam: MagicParamNameS): ScoutFate = {
//    ScoutFate(magicParams + magicParam)
//  }
//  def countMagicParams() = magicParams.size
//}
//
//case class ScoutFateBox(var fate: ScoutFate) {
//  def addMagicParam(codeLocation: MagicParamNameS): Unit = {
//    fate = fate.addMagicParam(codeLocation)
//  }
//  def countMagicParams(): Int = fate.countMagicParams()
//}

object FunctionScout {
//  // All closure structs start with this
//  val CLOSURE_STRUCT_NAME = "__Closure:"
//  // In a closure's environment, we also have this. This lets us easily know
//  // what the StructRef for a given closure is.
//  val CLOSURE_STRUCT_ENV_ENTRY_NAME = "__Closure"
//  // Name of anonymous substructs. They're more identified by their CodeLocation though.
//  val ANONYMOUS_SUBSTRUCT_NAME = "__AnonymousSubstruct"

  def scoutTopLevelFunction(file: String, functionP: FunctionP): FunctionS = {
    val FunctionP(
      Some(codeName),
      isExtern,
      isAbstract,
      isUserFunction,
      userSpecifiedIdentifyingRuneNames,
      templateRulesP,
      paramsP,
      maybeRetPPT,
      maybeBody0
    ) = functionP
    val codeLocation = CodeLocationS(functionP.pos.line, functionP.pos.column)
    val name = FunctionNameS(codeName, codeLocation)

    val userSpecifiedIdentifyingRunes =
      userSpecifiedIdentifyingRuneNames
        .map(identifyingRuneName => CodeRuneS(identifyingRuneName))
    val userRunesFromRules =
      RulePUtils.getOrderedRuneDeclarationsFromRulexesWithDuplicates(templateRulesP)
        .map(identifyingRuneName => CodeRuneS(identifyingRuneName))
    val userDeclaredRunes = userSpecifiedIdentifyingRunes ++ userRunesFromRules

    val functionEnv = FunctionEnvironment(name, None, userDeclaredRunes.toSet, paramsP.size)

    val rate = RuleStateBox(RuleState(name, 0))
    val userRulesS = RuleScout.translateRulexes(rate, functionEnv.allUserDeclaredRunes(), templateRulesP)

    val myStackFrameWithoutParams = StackFrame(name, functionEnv, None, noDeclarations)
    val (implicitRulesFromPatterns, explicitParamsPatterns1) =
      PatternScout.scoutPatterns(myStackFrameWithoutParams, rate, paramsP)
    val explicitParams1 = explicitParamsPatterns1.map(ParameterS)
    val captureDeclarations =
      explicitParams1
        .map(explicitParam1 => VariableDeclarations(PatternScout.getParameterCaptures(explicitParam1.pattern)))
        .foldLeft(noDeclarations)(_ ++ _)
    val myStackFrame = StackFrame(name, functionEnv, None, captureDeclarations)

    val identifyingRunes: List[IRuneS] =
      (
        userSpecifiedIdentifyingRunes //++
        // Patterns can't define runes
        // filledParamsP.flatMap(PatternPUtils.getOrderedIdentifyingRunesFromPattern)
      ).distinct

    val (implicitRulesFromRet, maybeRetCoordRune) =
      PatternScout.translateMaybeTypeIntoMaybeRune(
        userDeclaredRunes.toSet,
        rate,
        maybeRetPPT,
        CoordTypePR)

    val rulesS = userRulesS ++ implicitRulesFromPatterns ++ implicitRulesFromRet

    //    vassert(exportedTemplateParamNames.size == exportedTemplateParamNames.toSet.size)

    val body1 =
      if (isAbstract) {
        AbstractBody1
      } else if (isExtern) {
        ExternBody1
      } else {
        vassert(maybeBody0.nonEmpty)
        val (body1, _, List()) = scoutBody(myStackFrame, maybeBody0.get, captureDeclarations)
        CodeBody1(body1)
      }

    val allRunes =
      PredictorEvaluator.getAllRunes(
        name,
        identifyingRunes,
        rulesS,
        explicitParams1.map(_.pattern),
        maybeRetCoordRune)
    val Conclusions(knowableValueRunes, predictedTypeByRune) =
      PredictorEvaluator.solve(
        rulesS,
        explicitParams1.map(_.pattern))

    val isTemplate = knowableValueRunes != allRunes

    val maybePredictedType =
      if (isTemplate) {
        if ((identifyingRunes.toSet -- predictedTypeByRune.keySet).isEmpty) {
          Some(TemplateTypeSR(identifyingRunes.map(predictedTypeByRune), FunctionTypeSR))
        } else {
          None
        }
      } else {
        Some(FunctionTypeSR)
      }

    FunctionS(
      name,
      isUserFunction,
      identifyingRunes,
      allRunes,
      maybePredictedType,
      explicitParams1,
      maybeRetCoordRune,
      isTemplate,
      rulesS,
      body1)
  }

  def scoutLambda(
      parentStackFrame: StackFrame,
      lambdaFunction0: FunctionP):
  (FunctionS, VariableUses) = {
    val FunctionP(_, false, false, isUserFunction, userSpecifiedIdentifyingRuneNames, List(), paramsP, maybeRetPT, Some(body0)) = lambdaFunction0;
    val codeLocation = CodeLocationS(lambdaFunction0.pos.line, lambdaFunction0.pos.column)
    val userSpecifiedIdentifyingRunes: List[IRuneS] =
      userSpecifiedIdentifyingRuneNames.map(identifyingRuneName => CodeRuneS(identifyingRuneName))

    val lambdaName = LambdaNameS(/*parentStackFrame.name,*/ codeLocation)
    // Every lambda has a closure as its first arg, even if its empty
    val closureStructName = LambdaStructNameS(lambdaName)

    val rate = RuleStateBox(RuleState(lambdaName, 0))

    val functionEnv =
      FunctionEnvironment(
        lambdaName,
        Some(parentStackFrame.parentEnv),
        userSpecifiedIdentifyingRunes.toSet,
        paramsP.size)

    val myStackFrameWithoutParams = StackFrame(lambdaName, functionEnv, None, noDeclarations)

    val (implicitRulesFromParams, explicitParamPatterns1) =
      PatternScout.scoutPatterns(
        myStackFrameWithoutParams,
        rate,
        paramsP);
    val explicitParams1 = explicitParamPatterns1.map(ParameterS)
//    vassert(exportedTemplateParamNames.size == exportedTemplateParamNames.toSet.size)

    val closureParamName = ClosureParamNameS()

    val closureDeclaration =
      VariableDeclarations(List(VariableDeclaration(closureParamName, FinalP)))

    val paramDeclarations =
      explicitParams1.map(_.pattern)
        .map(pattern1 => VariableDeclarations(PatternScout.getParameterCaptures(pattern1)))
        .foldLeft(noDeclarations)(_ ++ _)

    // Don't add the closure to the environment, we don't want the user to be able
    // to access it.
    val myStackFrame = StackFrame(lambdaName, parentStackFrame.parentEnv, Some(parentStackFrame), paramDeclarations)

    val (body1, variableUses, lambdaMagicParamNames) =
      scoutBody(
        myStackFrame,
        body0,
        closureDeclaration ++ paramDeclarations)

    if (lambdaMagicParamNames.nonEmpty && (explicitParams1.nonEmpty)) {
      vfail("Cant have a lambda with _ and params");
    }

//    val closurePatternId = fate.nextPatternNumber();

    val closureParamTypeRune = rate.newImplicitRune()
    val rulesFromClosureParam =
      List(
        EqualsSR(
          TypedSR(closureParamTypeRune,CoordTypeSR),
          TemplexSR(OwnershippedST(BorrowP,AbsoluteNameST(closureStructName)))))
    val closureParamS = ParameterS(AtomSP(CaptureS(closureParamName,FinalP),None,closureParamTypeRune,None))

    val (magicParamsRules, magicParams) =
        lambdaMagicParamNames.map({
          case mpn @ MagicParamNameS(codeLocation) => {
            val magicParamRune = MagicParamRuneS(codeLocation)
            val ruleS = TypedSR(magicParamRune,CoordTypeSR)
            val paramS = ParameterS(AtomSP(CaptureS(mpn,FinalP),None,magicParamRune,None))
            (ruleS, paramS)
          }
        })
        .unzip

    val identifyingRunes =
      (
        userSpecifiedIdentifyingRunes ++
        // Patterns can't define runes
        // filledParamsP.flatMap(PatternPUtils.getOrderedIdentifyingRunesFromPattern) ++
        // Magic params always go on the end
        magicParams.map(_.pattern.coordRune)
      ).distinct

    val totalParams = closureParamS :: explicitParams1 ++ magicParams;

    val (implicitRulesFromReturn, maybeRetCoordRune) =
      PatternScout.translateMaybeTypeIntoMaybeRune(
        parentStackFrame.parentEnv.allUserDeclaredRunes(),
        rate,
        maybeRetPT,
        CoordTypePR)

    val rulesS = rulesFromClosureParam ++ magicParamsRules ++ implicitRulesFromParams ++ implicitRulesFromReturn

    val allRunes =
      PredictorEvaluator.getAllRunes(
        lambdaName,
        identifyingRunes,
        rulesS,
        explicitParams1.map(_.pattern),
        maybeRetCoordRune)
    val Conclusions(knowableValueRunes, predictedTypeByRune) =
      PredictorEvaluator.solve(
        rulesS,
        explicitParams1.map(_.pattern))
    val isTemplate = knowableValueRunes != allRunes

    val maybePredictedType =
      if (isTemplate) {
        if ((identifyingRunes.toSet -- predictedTypeByRune.keySet).isEmpty) {
          Some(TemplateTypeSR(identifyingRunes.map(predictedTypeByRune), FunctionTypeSR))
        } else {
          None
        }
      } else {
        Some(FunctionTypeSR)
      }

    val function1 =
      FunctionS(
        lambdaName,
        isUserFunction,
        identifyingRunes,
        allRunes,
        maybePredictedType,
        totalParams,
        maybeRetCoordRune,
        isTemplate,
        rulesS,
        CodeBody1(body1))
    (function1, variableUses)
  }

  // Returns:
  // - Body.
  // - Uses of parent variables.
  // - Magic params made/used inside.
  private def scoutBody(
    stackFrame: StackFrame,
    body0: BlockPE,
    paramDeclarations: VariableDeclarations):
  (BodySE, VariableUses, List[MagicParamNameS]) = {
    // There's an interesting consequence of calling this function here...
    // If we have a lone lookup node, like "m = Marine(); m;" then that
    // 'm' will be turned into an expression, which means that's how it's
    // destroyed. So, thats how we destroy things before their time.
    val (NormalResult(block1WithoutParamLocals), selfUses, childUses) =
      ExpressionScout.scoutBlock(stackFrame, body0)

    val paramLocals =
      paramDeclarations.vars.map({ declared =>
        LocalVariable1(
          declared.name,
          declared.variability,
          selfUses.isBorrowed(declared.name),
          selfUses.isMoved(declared.name),
          selfUses.isMutated(declared.name),
          childUses.isBorrowed(declared.name),
          childUses.isMoved(declared.name),
          childUses.isMutated(declared.name))
      })
    val block1WithParamLocals = BlockSE(block1WithoutParamLocals.locals ++ paramLocals, block1WithoutParamLocals.exprs)

    val allUses =
      selfUses.combine(childUses, {
        case (None, other) => other
        case (other, None) => other
        case (Some(NotUsed), other) => other
        case (other, Some(NotUsed)) => other
        // If we perhaps use it, and children perhaps use it, then say maybe used.
        case (Some(MaybeUsed), Some(MaybeUsed)) => Some(MaybeUsed)
        // If a child uses it, then count it as used
        case (Some(MaybeUsed), Some(Used)) => Some(Used)
        // If a child uses it, then count it as used
        case (Some(Used), Some(MaybeUsed)) => Some(Used)
        case (Some(Used), Some(Used)) => Some(Used)
      })

    // We're trying to figure out which variables from parent environments
    // we're using.
    // This is so we can remember in BodySE which variables we're using from
    // containing functions (so we can define the struct which we take in as
    // an implicit first parameter), and also so we can report those upward
    // so if we're using variables from our grandparent, our parent can know
    // that it needs to capture them for us.
    val usesOfParentVariables =
      allUses.uses.filter(use => {
        if (block1WithParamLocals.locals.exists(_.varName == use.name)) {
          // This is a use of a variable declared in this function.
          false
        } else {
          use.name match {
            case MagicParamNameS(_) => {
              // We're using a magic param, which we'll have in this function's params.
              false
            }
            case _ => {
              // This is a use of a variable from somewhere above.
              true
            }
          }
        }
      })

    val magicParams =
      allUses.uses.flatMap(_.name match {
        case mpn @ MagicParamNameS(_) => List(mpn)
        case _ => List()
      })

    (BodySE(usesOfParentVariables.map(_.name), block1WithParamLocals), VariableUses(usesOfParentVariables), magicParams)
  }

  def getContainingFunctionName(name: INameS): IFunctionDeclarationNameS = {
    vimpl()
//    val (functionNameStep, functionNameStepIndex) =
//      vassertSome(name.steps.zipWithIndex.reverse.collectFirst({
//        case (nameStep : IFunctionDeclarationNameS, nameStepIndex) => (nameStep, nameStepIndex)
//      }))
//    AbsoluteNameS(name.file, name.steps.slice(0, functionNameStepIndex), functionNameStep)
  }

  def scoutInterfaceMember(interfaceEnv: Environment, functionP: FunctionP): FunctionS = {
    val FunctionP(
      Some(codeName),
      false,
      _, // Ignore whether it thinks it's abstract or not
      true,
      userSpecifiedIdentifyingRuneNames,
      templateRulesP,
      paramsP,
      maybeReturnType,
      None) = functionP;
    val codeLocation = CodeLocationS(functionP.pos.line, functionP.pos.column)
    val funcName = FunctionNameS(codeName, codeLocation)
    val userSpecifiedIdentifyingRunes =
      userSpecifiedIdentifyingRuneNames
        .map(identifyingRuneName => CodeRuneS(identifyingRuneName))

    val identifyingRunes: List[IRuneS] =
      (
        userSpecifiedIdentifyingRunes// ++
        // Patterns can't define runes
        // filledParamsP.flatMap(PatternPUtils.getOrderedIdentifyingRunesFromPattern)
      ).distinct

    val rate = RuleStateBox(RuleState(funcName, 0))

    val userRulesS = RuleScout.translateRulexes(rate, interfaceEnv.allUserDeclaredRunes(), templateRulesP)

    val functionEnv = FunctionEnvironment(funcName, Some(interfaceEnv), userSpecifiedIdentifyingRunes.toSet, paramsP.size)
    val myStackFrame = StackFrame(funcName, functionEnv, None, noDeclarations)
    val (implicitRulesFromParams, patternsS) =
      PatternScout.scoutPatterns(myStackFrame, rate, paramsP)

    val paramsS = patternsS.map(ParameterS)

    val (implicitRulesFromRet, maybeReturnRune) =
      PatternScout.translateMaybeTypeIntoMaybeRune(
        interfaceEnv.allUserDeclaredRunes(),
        rate,
        maybeReturnType,
        CoordTypePR)

    val rulesS = userRulesS ++ implicitRulesFromParams ++ implicitRulesFromRet

    val allRunes =
      PredictorEvaluator.getAllRunes(
        funcName,
        identifyingRunes,
        rulesS,
        paramsS.map(_.pattern),
        maybeReturnRune)
    val Conclusions(knowableValueRunes, predictedTypeByRune) =
      PredictorEvaluator.solve(
        rulesS,
        paramsS.map(_.pattern))
    val isTemplate = knowableValueRunes != allRunes

    val maybePredictedType =
      if (isTemplate) {
        if ((identifyingRunes.toSet -- predictedTypeByRune.keySet).isEmpty) {
          Some(TemplateTypeSR(identifyingRunes.map(predictedTypeByRune), FunctionTypeSR))
        } else {
          None
        }
      } else {
        Some(FunctionTypeSR)
      }

    FunctionS(
      funcName,
      true,
      identifyingRunes,
      allRunes,
      maybePredictedType,
      paramsS,
      maybeReturnRune,
      isTemplate,
      rulesS,
      AbstractBody1);
  }

//
//  // returns seq num, new parameter, exported template names from typeless params, and capture names
//  private def scoutParameter(
//      fate: ScoutFate,
//      rulesS0: TemplateRulesS,
//      param0: ParameterPP):
//  (ScoutFate, TemplateRulesS, ParameterS) = {
//    val ParameterPP(maybeName, maybeVirtualityP, maybeCoordPP) = param0;
//
//
//
//    patternP match {
//      // Has name and type
//      case ParameterPP(Some(CaptureP(name, _)), _, Some(CoordPP(_, maybeOwnershipP, ReferendSP(None, None)))) => {
//        val patternId = fate.nextPatternNumber()
//
//        val param = ParameterS(patternId, name, pattern1)
//        (rulesS0, param)
//      }
//      // Has name, has no type
//      case ParameterSP(_, CoordSP(None, OwnershipSP(None, None), ReferendSP(None, None)), ValueSP(Some(CaptureS(name, _)), _)) => {
//        val templateParamTypeNumber = fate.nextTypeNumber()
//        val newTemplateParamName = "__T" + templateParamTypeNumber;
//        val patternId = fate.nextPatternNumber()
//        val param = ParameterS(patternId, name, pattern1)
//        (param, List(newTemplateParamName))
//      }
//      // Has no name, has type
//      case ParameterSP(_, CoordSP(Some(_), OwnershipSP(None, None), ReferendSP(None, None)), ValueSP(None, _)) => {
//        val num = fate.nextPatternNumber()
//        val name = "__arg_" + num
//        val patternId = fate.nextPatternNumber()
//        val param = ParameterS(patternId, name, pattern1)
//        (param, List())
//      }
//      // Has no name nor type
//      case ParameterSP(_, CoordSP(None, OwnershipSP(None, None), ReferendSP(None, None)), ValueSP(None, _)) => {
//        val num = fate.nextPatternNumber()
//        val name = "__arg_" + num
//        val templateParamTypeNumber = fate.nextTypeNumber()
//        val newTemplateParamName = "__T" + templateParamTypeNumber;
//        val patternId = fate.nextPatternNumber()
//        val param = ParameterS(patternId, name, pattern1)
//        (param, List(newTemplateParamName))
//      }
//      case _ => {
//        vfail("curiosity") // when does this happen
//        val num = fate.nextPatternNumber()
//        val name = "__arg_" + num
//        val patternId = fate.nextPatternNumber()
//        val param = ParameterS(patternId, name, pattern1)
//        (param, List())
//      }
//    }
//  }
}
