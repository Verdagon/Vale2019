package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.ExpressionScout.NormalResult
import net.verdagon.vale.scout.Scout.noDeclarations
import net.verdagon.vale.scout.patterns.PatternScout.{InitialRulesAndRunes, RuleState, RuleStateBox}
import net.verdagon.vale.scout.patterns._
import net.verdagon.vale.scout.predictor.Conclusions
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.scout.templatepredictor.PredictorEvaluator
import net.verdagon.vale.{vassert, vfail}

import scala.collection.immutable.{List, Range}

// Note, if we enter into a block, the statements inside the block affect the fate, just
// as if we entered anything else like a pack or sequence or whatever. It's not treated
// specially.
case class ScoutFate(
  private val numPatterns: Int,
  private val numLambdas: Int,
  private val numTypes: Int,
  private val numMagicParams: Int,
  private val numLets: Int
) {
  def nextPatternNumber(): (ScoutFate, Int) = {
    (ScoutFate(numPatterns + 1, numLambdas, numTypes, numMagicParams, numLets), numPatterns + 1)
  }
  def nextLambdaNumber(): (ScoutFate, Int) = {
    (ScoutFate(numPatterns, numLambdas + 1, numTypes, numMagicParams, numLets), numLambdas + 1)
  }
  def nextTypeNumber(): (ScoutFate, Int) = {
    (ScoutFate(numPatterns, numLambdas, numTypes + 1, numMagicParams, numLets), numTypes + 1)
  }
  def nextMagicParamNumber(): (ScoutFate, Int) = {
    (ScoutFate(numPatterns, numLambdas, numTypes, numMagicParams + 1, numLets), numMagicParams)
  }
  def nextLetNumber(): (ScoutFate, Int) = {
    (ScoutFate(numPatterns + 1, numLambdas, numTypes, numMagicParams, numLets + 1), numPatterns)
  }
  def countMagicParams() = numMagicParams
}

case class ScoutFateBox(var fate: ScoutFate) {
  def nextPatternNumber(): Int = {
    val (newFate, num) = fate.nextPatternNumber()
    fate = newFate
    num
  }
  def nextLambdaNumber(): Int = {
    val (newFate, num) = fate.nextLambdaNumber()
    fate = newFate
    num
  }
  def nextTypeNumber(): Int = {
    val (newFate, num) = fate.nextTypeNumber()
    fate = newFate
    num
  }
  def nextMagicParamNumber(): Int = {
    val (newFate, num) = fate.nextMagicParamNumber()
    fate = newFate
    num
  }
  def nextLetNumber(): Int = {
    val (newFate, num) = fate.nextLetNumber()
    fate = newFate
    num
  }
  def countMagicParams(): Int = fate.countMagicParams()
}

object FunctionScout {
  // All closure structs start with this
  val CLOSURE_STRUCT_NAME = "__Closure:"
  // In a closure's environment, we also have this. This lets us easily know
  // what the StructRef for a given closure is.
  val CLOSURE_STRUCT_ENV_ENTRY_NAME = "__Closure"

  def fillParams(patterns: List[PatternPP]): List[PatternPP] = {
    patterns.zipWithIndex.map({ case (pattern, paramIndex) =>
      val PatternPP(maybeCapture, maybeTemplex, maybeVirtuality, maybeDestructure) = pattern

      val capture = maybeCapture.getOrElse(CaptureP(Scout.unnamedParamNamePrefix + paramIndex, FinalP))

      val templex = maybeTemplex.getOrElse(AnonymousRunePPT())
      val filledTemplex = fillTemplex(paramIndex, templex)

      // We don't need to fill anything from the virtuality, because it can't have any
      // identifying params, see NIPFO.

      PatternPP(Some(capture), Some(filledTemplex), maybeVirtuality, maybeDestructure)
    })
  }

  def fillTemplex(paramIndex: Int, originalTemplex: ITemplexPPT): ITemplexPPT = {
    val anonymousRuneIndex0 = 0
    val paramIndexBox0 = IntBox(0)
    val filledTemplex =
      PatternPUtils.traverseTemplex[IntBox, Int](
        paramIndexBox0,
        anonymousRuneIndex0,
        originalTemplex,
        (paramIndexBox: IntBox, anonymousRuneIndex0: Int, templex: ITemplexPPT) => {
          templex match {
            case AnonymousRunePPT() => {
              val newTemplex = RunePPT(Scout.unrunedParamRunePrefix + paramIndexBox.num + "_" + anonymousRuneIndex0)
              paramIndexBox.num = paramIndexBox.num + 1
              newTemplex
            }
            case other => other
          }
        })
    filledTemplex
  }

  def scoutTopLevelFunction(
      functionP: FunctionP):
  FunctionS = {
    val FunctionP(
      Some(name),
      isExtern,
      isAbstract,
      isUserFunction,
      userSpecifiedIdentifyingRunes,
      templateRulesP,
      unfilledParamsP,
      maybeRetPPT,
      maybeBody0
    ) = functionP;
    val codeLocation = CodeLocation("userInput.vale", functionP.pos.line, functionP.pos.column)

    val filledParamsP = fillParams(unfilledParamsP)

    val fate = ScoutFateBox(ScoutFate(0, 0, 0, 0, 0))

    val initialRulesAndRunes =
      InitialRulesAndRunes(
        userSpecifiedIdentifyingRunes,
        templateRulesP,
        filledParamsP,
        maybeRetPPT)

    val rate = RuleStateBox(RuleState(RuleScout.translateRulexes(templateRulesP)))

    val explicitParamsPatterns1 =
      PatternScout.scoutPatterns(
        initialRulesAndRunes,
        fate,
        rate,
        filledParamsP,
        Some(Scout.unnamedParamNamePrefix),
        Some(Scout.unrunedParamRunePrefix))
    val explicitParams1 = explicitParamsPatterns1.map(ParameterS)

    val identifyingRunes =
      (
        userSpecifiedIdentifyingRunes ++
        filledParamsP.flatMap(PatternPUtils.getOrderedIdentifyingRunesFromPattern)
      ).distinct

    val maybeRetCoordRune =
      PatternScout.translateMaybeTypeIntoMaybeRune(
        initialRulesAndRunes,
        rate,
        maybeRetPPT,
        CoordTypePR,
        Some("__Ret"))

    //    vassert(exportedTemplateParamNames.size == exportedTemplateParamNames.toSet.size)

    val captureDeclarations =
      explicitParams1
        .map(explicitParam1 => VariableDeclarations(PatternScout.getParameterCaptures(explicitParam1.pattern)))
        .foldLeft(noDeclarations)(_ ++ _)

    val myStackFrame = StackFrame(None, filledParamsP.size, captureDeclarations)
    val body1 =
      if (isAbstract) {
        AbstractBody1
      } else if (isExtern) {
        ExternBody1
      } else {
        vassert(maybeBody0.nonEmpty)
        val (body1, _) =
          scoutBody(
            name, fate, myStackFrame, maybeBody0.get, captureDeclarations)
        CodeBody1(body1)
      }

    val allRunes =
      PredictorEvaluator.getAllRunes(
        identifyingRunes,
        rate.rate.rulexesS,
        explicitParams1.map(_.pattern),
        maybeRetCoordRune)
    val Conclusions(knowableValueRunes, predictedTypeByRune) =
      PredictorEvaluator.solve(
        rate.rate.rulexesS,
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
      codeLocation,
      name,
      List(name),
      0,
      isUserFunction,
      identifyingRunes,
      allRunes,
      maybePredictedType,
      explicitParams1,
      maybeRetCoordRune,
      isTemplate,
      rate.rate.rulexesS,
      body1)
  }

  def scoutLambda(
      tlfName: String,
      fate: ScoutFateBox,
      parentStackFrame: Option[StackFrame],
      lambdaFunction0: FunctionP):
  (FunctionS, VariableUses) = {
    val FunctionP(_, false, false, isUserFunction, userSpecifiedIdentifyingRunes, List(), unfilledParamsP, maybeRetPT, Some(body0)) = lambdaFunction0;
    val codeLocation = CodeLocation("userInput.vale", lambdaFunction0.pos.line, lambdaFunction0.pos.column)

    val filledParamsP = fillParams(unfilledParamsP)

    val funcId = fate.nextLambdaNumber()
    val funcName = tlfName + ":lam" + funcId

    val initialRulesAndRunes =
      InitialRulesAndRunes(userSpecifiedIdentifyingRunes, List(), filledParamsP, maybeRetPT)

    val rate = RuleStateBox(RuleState(List()))

    val ScoutFate(numPatternsBefore, numLambdasBefore, numTypesBefore, numMagicParamsBefore, numLetsBefore) = fate.fate
    val lambdaFate = ScoutFateBox(ScoutFate(0, numLambdasBefore, numTypesBefore, 0, 0))

    val explicitParamPatterns1 =
      PatternScout.scoutPatterns(
        initialRulesAndRunes,
        lambdaFate,
        rate,
        filledParamsP,
        Some(Scout.unnamedParamNamePrefix),
        Some(Scout.unrunedParamRunePrefix));
    val explicitParams1 = explicitParamPatterns1.map(ParameterS)
//    vassert(exportedTemplateParamNames.size == exportedTemplateParamNames.toSet.size)

    val closureDeclaration =
      VariableDeclarations(Set(VariableDeclaration("__closure", FinalP)))

    val paramDeclarations =
      explicitParams1.map(_.pattern)
        .map(pattern1 => VariableDeclarations(PatternScout.getParameterCaptures(pattern1)))
        .foldLeft(noDeclarations)(_ ++ _)

    // Don't add the closure to the environment, we don't want the user to be able
    // to access it.
    val myStackFrame = StackFrame(parentStackFrame, explicitParams1.size, paramDeclarations)

    val (body1, variableUses) =
      scoutBody(
        tlfName,
        lambdaFate,
        myStackFrame,
        body0,
        closureDeclaration ++ paramDeclarations)

    if ((lambdaFate.countMagicParams() != 0) && (explicitParams1.nonEmpty)) {
      vfail("Cant have a lambda with _ and params");
    }

    // Every lambda has a closure as its first arg, even if its empty
    val closureStructName = CLOSURE_STRUCT_NAME + funcName
//    val (closurePatternId) = fate.nextPatternNumber();

    // We're basically trying to add `__closure: &__Closure<main>:lam1`
    val closureParamAtomSP =
      PatternScout.translatePattern(
        initialRulesAndRunes,
        lambdaFate,
        rate,
        PatternPP(
          Some(CaptureP("__closure", FinalP)),
          Some(OwnershippedPPT(BorrowP, NamePPT(closureStructName))),
          None,
          None),
        Some("__closure"),
        Some("__C"))
    val closureParam1 = ParameterS(closureParamAtomSP)

    val magicParams =
      (0 until lambdaFate.countMagicParams())
        .foldLeft(List[ParameterS]())({
          case (previousParams1, magicParamIndex) => {
            val paramIndex = explicitParams1.size + magicParamIndex
            val paramS =
              PatternScout.translatePattern(
                initialRulesAndRunes,
                lambdaFate,
                rate,
                PatternPP(None, None, None, None),
                Some(Scout.unnamedParamNamePrefix + paramIndex),
                Some(Scout.unrunedParamRunePrefix + paramIndex))
            previousParams1 :+ ParameterS(paramS)
          }
        })

    val identifyingRunes =
      (
        userSpecifiedIdentifyingRunes ++
        filledParamsP.flatMap(PatternPUtils.getOrderedIdentifyingRunesFromPattern) ++
        // Magic params always go on the end
        magicParams.map(_.pattern.coordRune)
      ).distinct

    val totalParams = closureParam1 :: explicitParams1 ++ magicParams;

    val maybeRetCoordRune =
      PatternScout.translateMaybeTypeIntoMaybeRune(
        initialRulesAndRunes,
        rate,
        maybeRetPT,
        CoordTypePR,
        Some("__R"))

    val allRunes =
      PredictorEvaluator.getAllRunes(
        identifyingRunes,
        rate.rate.rulexesS,
        explicitParams1.map(_.pattern),
        maybeRetCoordRune)
    val Conclusions(knowableValueRunes, predictedTypeByRune) =
      PredictorEvaluator.solve(
        rate.rate.rulexesS,
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


    val ScoutFate(_, numLambdasAfter, numTypesAfter, _, _) = lambdaFate.fate
    fate.fate = ScoutFate(numPatternsBefore, numLambdasAfter, numTypesAfter, numMagicParamsBefore, numLetsBefore)

    val function1 =
      FunctionS(
        codeLocation,
        funcName,
        List(tlfName, funcName), // Soon, we'll want to put namespaces in here
        funcId,
        isUserFunction,
        identifyingRunes,
        allRunes,
        maybePredictedType,
        totalParams,
        maybeRetCoordRune,
        isTemplate,
        rate.rate.rulexesS,
        CodeBody1(body1))
    (function1, variableUses)
  }

  private def scoutBody(
    tlfName: String, fate: ScoutFateBox, stackFrame: StackFrame, body0: BlockPE, paramDeclarations: VariableDeclarations):
  (BodySE, VariableUses) = {
    // There's an interesting consequence of calling this function here...
    // If we have a lone lookup node, like "m = Marine(); m;" then that
    // 'm' will be turned into an expression, which means that's how it's
    // destroyed. So, thats how we destroy things before their time.
    val (NormalResult(block1WithoutParamLocals), selfUses, childUses) =
      ExpressionScout.scoutBlock(
        tlfName, fate, stackFrame, body0)

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

    val allUses = selfUses.combine(childUses, {
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

    val closuredNames =
      allUses.allUsedNames.map({ name =>
        val isClosure =
          stackFrame.getJumps(name) match {
            case None => false
            case Some(0) => false
            case Some(_) => true
          };
        if (isClosure) Set(name) else Set()
      })
          .foldLeft(Set[String]())(_ ++ _);

    (BodySE(closuredNames, block1WithParamLocals), allUses)
  }

  def scoutInterfaceMember(functionP: FunctionP): FunctionS = {
    val FunctionP(
      maybeName,
      false,
      _, // Ignore whether it thinks it's abstract or not
      true,
      userSpecifiedIdentifyingRunes,
      templateRulesP,
      unfilledParamsP,
      maybeReturnType,
      None) = functionP;
    val codeLocation = CodeLocation("userInput.vale", functionP.pos.line, functionP.pos.column)

    val filledParamsP = fillParams(unfilledParamsP)

    if (maybeName.isEmpty) {
      vfail("no");
    }

    val initialRulesAndRunes =
      InitialRulesAndRunes(userSpecifiedIdentifyingRunes, templateRulesP, filledParamsP, maybeReturnType)

    val identifyingRunes =
      (
        userSpecifiedIdentifyingRunes ++
        filledParamsP.flatMap(PatternPUtils.getOrderedIdentifyingRunesFromPattern)
      ).distinct

    val fate = ScoutFateBox(ScoutFate(0, 0, 0, 0, 0))
    val rate = RuleStateBox(RuleState(List()))
    val patternsS =
      PatternScout.scoutPatterns(
        initialRulesAndRunes,
        fate,
        rate,
        filledParamsP,
        Some("__par"),
        Some("__Par"))

    // Theres no body, no need to keep the fate around.
    val _ = fate

    val paramsS = patternsS.map(ParameterS)

    val maybeReturnRune =
      PatternScout.translateMaybeTypeIntoMaybeRune(
        initialRulesAndRunes,
        rate,
        maybeReturnType,
        CoordTypePR,
        Some("__Ret"))

    val allRunes =
      PredictorEvaluator.getAllRunes(
        identifyingRunes,
        rate.rate.rulexesS,
        paramsS.map(_.pattern),
        maybeReturnRune)
    val Conclusions(knowableValueRunes, predictedTypeByRune) =
      PredictorEvaluator.solve(
        rate.rate.rulexesS,
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
      codeLocation,
      maybeName.get,
      List(maybeName.get),
      0,
      true,
      identifyingRunes,
      allRunes,
      maybePredictedType,
      paramsS,
      maybeReturnRune,
      isTemplate,
      rate.rate.rulexesS,
      AbstractBody1);
  }

  def simplifyParams(params1: List[AtomSP]): List[SimpleParameter1] = {
    params1.map({
      case param1 @ AtomSP(Some(CaptureP(name, _)), virtuality, coordRune, _) => {
        SimpleParameter1(Some(param1), name, virtuality, NameST(coordRune))
      }
    })
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
//        val (patternId) = fate.nextPatternNumber()
//
//        val param = ParameterS(patternId, name, pattern1)
//        (rulesS0, param)
//      }
//      // Has name, has no type
//      case ParameterSP(_, CoordSP(None, OwnershipSP(None, None), ReferendSP(None, None)), ValueSP(Some(CaptureS(name, _)), _)) => {
//        val (templateParamTypeNumber) = fate.nextTypeNumber()
//        val newTemplateParamName = "__T" + templateParamTypeNumber;
//        val (patternId) = fate.nextPatternNumber()
//        val param = ParameterS(patternId, name, pattern1)
//        (param, List(newTemplateParamName))
//      }
//      // Has no name, has type
//      case ParameterSP(_, CoordSP(Some(_), OwnershipSP(None, None), ReferendSP(None, None)), ValueSP(None, _)) => {
//        val (num) = fate.nextPatternNumber()
//        val name = "__arg_" + num
//        val (patternId) = fate.nextPatternNumber()
//        val param = ParameterS(patternId, name, pattern1)
//        (param, List())
//      }
//      // Has no name nor type
//      case ParameterSP(_, CoordSP(None, OwnershipSP(None, None), ReferendSP(None, None)), ValueSP(None, _)) => {
//        val (num) = fate.nextPatternNumber()
//        val name = "__arg_" + num
//        val (templateParamTypeNumber) = fate.nextTypeNumber()
//        val newTemplateParamName = "__T" + templateParamTypeNumber;
//        val (patternId) = fate.nextPatternNumber()
//        val param = ParameterS(patternId, name, pattern1)
//        (param, List(newTemplateParamName))
//      }
//      case _ => {
//        vfail("curiosity") // when does this happen
//        val (num) = fate.nextPatternNumber()
//        val name = "__arg_" + num
//        val (patternId) = fate.nextPatternNumber()
//        val param = ParameterS(patternId, name, pattern1)
//        (param, List())
//      }
//    }
//  }
}
