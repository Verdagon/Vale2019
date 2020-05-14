package net.verdagon.vale.scout

import net.verdagon.vale.parser._
import net.verdagon.vale.{scout, vfail, vimpl, vwat}
import net.verdagon.vale.scout.Scout.{noDeclarations, noVariableUses}
import net.verdagon.vale.scout.patterns.{LetRuleState, PatternScout, RuleState, RuleStateBox}
import net.verdagon.vale.scout.predictor.Conclusions
import net.verdagon.vale.scout.rules.RuleScout
import net.verdagon.vale.scout.templatepredictor.PredictorEvaluator

object ExpressionScout {
  sealed trait IScoutResult[+T <: IExpressionSE]
  // Will contain the address of a local.
  case class LocalLookupResult(name: IVarNameS) extends IScoutResult[IExpressionSE]
  // Looks up a function.
  case class FunctionLookupResult(name: GlobalFunctionFamilyNameS) extends IScoutResult[IExpressionSE]
  // Anything else, such as:
  // - Result of a function call
  // - Address inside a struct
  case class NormalResult[+T <: IExpressionSE](expr: T) extends IScoutResult[T]


  def scoutBlock(
    stackFrame: StackFrame,
    BlockPE: BlockPE):
  (IScoutResult[BlockSE], VariableUses, VariableUses) = {
    val (innerDeclarations, elements1, selfUses, childUses) =
      scoutElementsAsExpressions(stackFrame, BlockPE.elements)

    val locals =
      innerDeclarations.vars.map({ declared =>
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

    val selfUsesOfThingsFromAbove =
      VariableUses(selfUses.uses.filter(selfUseName => !locals.map(_.varName).contains(selfUseName)))
    val childUsesOfThingsFromAbove =
      VariableUses(childUses.uses.filter(selfUseName => !locals.map(_.varName).contains(selfUseName)))

    // Notice how the fate is continuing on
    (NormalResult(BlockSE(locals, elements1)), selfUsesOfThingsFromAbove, childUsesOfThingsFromAbove)
  }

  // Returns:
  // - new seq num
  // - declared variables
  // - new expression
  // - variable uses by self
  // - variable uses by child blocks
  private def scoutExpression(stackFrame: StackFrame,  expr: IExpressionPE):
      (VariableDeclarations, IScoutResult[IExpressionSE], VariableUses, VariableUses) = {
    expr match {
      case VoidPE() => (noDeclarations, NormalResult(VoidSE()), noVariableUses, noVariableUses)
      case lam @ LambdaPE(_) => {
        val (function1, childUses) =
          FunctionScout.scoutLambda(stackFrame, lam.function)

        // See maybify() for why we need this.
        val childMaybeUses = childUses.maybify()

        (noDeclarations, NormalResult(FunctionSE(function1)), noVariableUses, childMaybeUses)
      }
      case LendPE(innerPE) => {
        val (declareds, inner1, innerSelfUses, innerChildUses) =
          scoutExpressionAndCoerce(stackFrame, innerPE, borrowIfLookupResult = true)
       (declareds, NormalResult(ExpressionLendSE(inner1)), innerSelfUses, innerChildUses)
      }
      case ReturnPE(innerPE) => {
        val (declareds, inner1, innerSelfUses, innerChildUses) =
          scoutExpressionAndCoerce(stackFrame, innerPE, borrowIfLookupResult = false)
        (declareds, NormalResult(ReturnSE(inner1)), innerSelfUses, innerChildUses)
      }
      case PackPE(elements) => {
        val (declareds, elements1, selfUses, childUses) =
          scoutElementsAsExpressions(stackFrame, elements)
        (declareds, NormalResult(PackSE(elements1)), selfUses, childUses)
      }
      case IntLiteralPE(value) => (noDeclarations, NormalResult(IntLiteralSE(value)), noVariableUses, noVariableUses)
      case BoolLiteralPE(value) => (noDeclarations, NormalResult(BoolLiteralSE(value)), noVariableUses, noVariableUses)
      case StrLiteralPE(value) => (noDeclarations, NormalResult(StrLiteralSE(value)), noVariableUses, noVariableUses)
      case FloatLiteralPE(value) => (noDeclarations, NormalResult(FloatLiteralSE(value)), noVariableUses, noVariableUses)

      case FunctionCallPE(DotPE(container, memberLookup, true), PackPE(methodArgs), borrowCallable) => {
        // Correct method calls like anExpr.bork(4) from FunctionCall(Dot(anExpr, bork), List(4))
        // to FunctionCall(bork, List(anExpr, 4))
        val newExprP = FunctionCallPE(memberLookup, PackPE(LendPE(container) :: methodArgs), borrowCallable)
        // Try again, with this new transformed expression.
        scoutExpression(stackFrame, newExprP)
      }
      case FunctionCallPE(DotPE(container, memberLookup, false), PackPE(methodArgs), borrowCallable) => {
        // Correct method calls like anExpr.bork(4) from FunctionCall(Dot(anExpr, bork), List(4))
        // to FunctionCall(bork, List(anExpr, 4))
        val newExprP = FunctionCallPE(memberLookup, PackPE(container :: methodArgs), borrowCallable)
        // Try again, with this new transformed expression.
        scoutExpression(stackFrame, newExprP)
      }
      case mpl @ MagicParamLookupPE() => {
        val name = MagicParamNameS(CodeLocationS(mpl.pos.line, mpl.pos.column))
        val lookup = LocalLookupResult(name)
        val declarations = VariableDeclarations(List(VariableDeclaration(lookup.name, FinalP)))
        (declarations, lookup, noVariableUses.markMoved(name), noVariableUses)
      }
      case LookupPE(name, List()) => {
        val (lookup, declarations) =
          stackFrame.findVariable(name) match {
            case None => (FunctionLookupResult(GlobalFunctionFamilyNameS(name)), noDeclarations)
            case Some(fullName) => (LocalLookupResult(fullName), noDeclarations)
          }
        (declarations, lookup, noVariableUses, noVariableUses)
      }
      case LookupPE(templateName, templateArgs) => {
        val result =
          NormalResult(
            TemplateSpecifiedLookupSE(
              templateName,
              templateArgs.map(TemplexScout.translateTemplex(stackFrame.parentEnv.allUserDeclaredRunes(), _))))
        (noDeclarations, result, noVariableUses, noVariableUses)
      }
      case FunctionCallPE(callablePE, argsPackPE, borrowCallable) => {
        if (borrowCallable != true) {
          // Havent yet implemented something like myFunctor^(4), should be easy piping though
          vimpl("Havent implemented moving callable yet")
        }
        val (callableDeclareds, callable1, callableSelfUses, callableChildUses) =
          scoutExpressionAndCoerce(stackFrame, callablePE, true)
        val (argsDeclareds, args1, argsSelfUses, argsChildUses) =
          scoutElementsAsExpressions(stackFrame, argsPackPE.elements)
        val result = NormalResult(FunctionCallSE(callable1, PackSE(args1)))
        (callableDeclareds ++ argsDeclareds, result, callableSelfUses.thenMerge(argsSelfUses), callableChildUses.thenMerge(argsChildUses))
      }
      case SequencePE(elementsPE) => {
        val (declareds, elements1, selfUses, childUses) =
          scoutElementsAsExpressions(stackFrame, elementsPE)
        (declareds, NormalResult(scout.SequenceESE(elements1)), selfUses, childUses)
      }
      case b @ BlockPE(_) => {
        val (result, selfUses, childUses) =
          scoutBlock(stackFrame, b)
        (noDeclarations, result, selfUses, childUses)
      }
      case IfPE(condition, thenBody, elseBody) => {
        val (NormalResult(cond1), condUses, condChildUses) =
          scoutBlock(stackFrame, condition)
        val (NormalResult(then1), thenUses, thenChildUses) =
          scoutBlock(stackFrame, thenBody)
        val (NormalResult(else1), elseUses, elseChildUses) =
          scoutBlock(stackFrame, elseBody)

        val selfCaseUses = thenUses.branchMerge(elseUses)
        val selfUses = condUses.thenMerge(selfCaseUses);
        val childCaseUses = thenChildUses.branchMerge(elseChildUses)
        val childUses = condChildUses.thenMerge(childCaseUses);

        (noDeclarations, NormalResult(IfSE(cond1, then1, else1)), selfUses, childUses)
      }
      case WhilePE(condition, body) => {
        val (NormalResult(cond1), condSelfUses, condChildUses) =
          scoutBlock(stackFrame, condition)

        // Ignoring exported names
        val (NormalResult(body1), bodySelfUses, bodyChildUses) =
          scoutBlock(stackFrame, body)

        val bodySelfMaybeUses = bodySelfUses.maybify()
        val bodyChildMaybeUses = bodyChildUses.maybify()

        // Condition's uses isn't sent through a branch merge because the condition
        // is *always* evaluated (at least once).
        val selfUses = condSelfUses.thenMerge(bodySelfMaybeUses)
        val childUses = condChildUses.thenMerge(bodyChildMaybeUses)

        (noDeclarations, NormalResult(WhileSE(cond1, body1)), selfUses, childUses)
      }
      case let @ LetPE(rulesP, patternP, exprPE) => {
        val codeLocation = CodeLocationS(let.pos.line, let.pos.column)
        val (declarations, expr1, selfUses, childUses) =
          scoutExpressionAndCoerce(stackFrame, exprPE, borrowIfLookupResult = false);

        val letFullName = LetNameS(codeLocation)

        val ruleState = RuleStateBox(LetRuleState(letFullName, codeLocation, 0))
        val userRulesS =
          RuleScout.translateRulexes(
            ruleState, stackFrame.parentEnv.allUserDeclaredRunes(), rulesP)
        val (implicitRulesS, patternS) =
          PatternScout.translatePattern(
            stackFrame,
            ruleState,
            patternP)
        val rulesS = userRulesS ++ implicitRulesS

        val allRunes = PredictorEvaluator.getAllRunes(letFullName, List(), rulesS, List(patternS), None)

        // See MKKRFA
        val knowableRunesFromAbove = stackFrame.parentEnv.allUserDeclaredRunes()
        val allUnknownRunes = allRunes -- knowableRunesFromAbove
        val Conclusions(knowableValueRunes, predictedTypeByRune) =
          PredictorEvaluator.solve(Set(), rulesS, List())
        val localRunes = allRunes -- knowableRunesFromAbove

        val declarationsFromPattern = VariableDeclarations(PatternScout.getParameterCaptures(patternS))
        (declarations ++ declarationsFromPattern, NormalResult(LetSE(rulesS, allUnknownRunes, localRunes, patternS, expr1)), selfUses, childUses)
      }
      case MutatePE(destinationExprPE, sourceExprPE) => {
        val (exportedNames1, sourceExpr1, sourceInnerSelfUses, sourceChildUses) =
          scoutExpressionAndCoerce(stackFrame, sourceExprPE, borrowIfLookupResult = false);
        val (exportedNames2, destinationResult1, destinationSelfUses, destinationChildUses) =
          scoutExpression(stackFrame, destinationExprPE);
        val (mutateExpr1, sourceSelfUses) =
          destinationResult1 match {
            case LocalLookupResult(name) => {
              (LocalMutateSE(name, sourceExpr1), sourceInnerSelfUses.markMutated(name))
            }
            case FunctionLookupResult(_) => {
              vfail("Cant mutate a function")
//              (GlobalMutateSE(name, sourceExpr1), sourceInnerSelfUses.markMutated(name))
            }
            case NormalResult(destinationExpr1) => {
              (ExprMutateSE(destinationExpr1, sourceExpr1), sourceInnerSelfUses)
            }
          }
        (exportedNames1 ++ exportedNames2, NormalResult(mutateExpr1), sourceSelfUses.thenMerge(destinationSelfUses), sourceChildUses.thenMerge(destinationChildUses))
      }
      case DotPE(containerExprPE, LookupPE(memberName, templateArgs), borrowContainer) => {
        if (templateArgs != List()) {
          // such as myStruct.someField<Foo>.
          // Can't think of a good use for it yet.
          vimpl("havent implemented looking up templated members yet")
        }
        val (exportedNames1, containerExpr, selfUses, childUses) =
          scoutExpressionAndCoerce(stackFrame, containerExprPE, borrowContainer);
        (exportedNames1, NormalResult(DotSE(containerExpr, memberName, borrowContainer)), selfUses, childUses)
      }
      case DotCallPE(containerExprPE, indexExprPE, borrowContainer) => {
        val (exportedNames1, containerExpr1, containerSelfUses, containerChildUses) =
          scoutExpressionAndCoerce(stackFrame, containerExprPE, borrowIfLookupResult = borrowContainer);
        val (exportedNames2, indexExpr1, indexSelfUses, indexChildUses) =
          scoutExpressionAndCoerce(stackFrame, indexExprPE, borrowIfLookupResult = false);
        val dot1 = DotCallSE(containerExpr1, indexExpr1)
        (exportedNames1 ++ exportedNames2, NormalResult(dot1), containerSelfUses.thenMerge(indexSelfUses), containerChildUses.thenMerge(indexChildUses))
      }
    }
  }

  def scoutExpressionAndCoerce(
      stackFramePE: StackFrame,  exprPE: IExpressionPE, borrowIfLookupResult: Boolean):
  (VariableDeclarations, IExpressionSE, VariableUses, VariableUses) = {
    val (namesFromInsideFirst, firstResult1, firstInnerSelfUses, firstChildUses) =
      scoutExpression(stackFramePE, exprPE);
    val (firstExpr1, firstSelfUses) =
      firstResult1 match {
        case LocalLookupResult(name) => {
          val uses =
            if (borrowIfLookupResult) {
              firstInnerSelfUses.markBorrowed(name)
            } else {
              firstInnerSelfUses.markMoved(name)
            }
          (LocalLoadSE(name, borrowIfLookupResult), uses)
        }
        case FunctionLookupResult(name) => {
          (FunctionLoadSE(name), firstInnerSelfUses)
        }
        case NormalResult(innerExpr1) => {
          (innerExpr1, firstInnerSelfUses)
        }
      }
    (namesFromInsideFirst, firstExpr1, firstSelfUses, firstChildUses)
  }

  // Need a better name for this...
  // It's more like, scout elements as non-lookups, in other words,
  // if we get lookups then coerce them into moves.
  def scoutElementsAsExpressions(stackFramePE: StackFrame,  exprs: List[IExpressionPE]):
  (VariableDeclarations, List[IExpressionSE], VariableUses, VariableUses) = {
    exprs match {
      case Nil => (noDeclarations, Nil, noVariableUses, noVariableUses)
      case firstPE :: restPE => {
        val (firstDeclareds, firstExpr1, firstSelfUses, firstChildUses) =
          scoutExpressionAndCoerce(stackFramePE, firstPE, borrowIfLookupResult = false)
        val stackFrame1 = stackFramePE ++ firstDeclareds;
        val (restDeclareds, rest1, restSelfUses, restChildUses) =
          scoutElementsAsExpressions(stackFrame1, restPE);
        (firstDeclareds ++ restDeclareds, firstExpr1 :: rest1, firstSelfUses.thenMerge(restSelfUses), firstChildUses.thenMerge(restChildUses))
      }
    }
  }

//  private def flattenScramble(elements: List[Expression1]): List[Expression1] = {
//    elements.map((expr: Expression1) => {
//      expr match {
//        case Scramble1(elementElements) => flattenScramble(elementElements)
//        case _ => List(expr)
//      }
//    }).foldLeft(List[Expression1]())(_ ++ _)
//  }
}
