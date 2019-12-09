package net.verdagon.radonc.templar.function


import net.verdagon.radonc.astronomer.{BFunctionA, BodyAE}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.parser.CaptureP
import net.verdagon.radonc._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.AtomSP
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.env._
import net.verdagon.radonc.templar.templata.TemplataTemplar

import scala.collection.immutable.{List, Nil, Set}

object BodyTemplar {

  def declareAndEvaluateFunctionBody(
      funcOuterEnv: FunctionEnvironment,
      temputs: TemputsBox,
      bfunction1: BFunctionA,
      params2: List[Parameter2],
      isDestructor: Boolean):
  (FunctionEnvironment, FunctionHeader2, Block2) = {
    val BFunctionA(function1, name, _) = bfunction1;
    val functionFullName = funcOuterEnv.fullName

    function1.maybeRetCoordRune match {
      case None => {
        val banner = FunctionBanner2(Some(function1), functionFullName, params2)
        val (env1, body2, returnsFromRets) =
          evaluateFunctionBody(funcOuterEnv, temputs, bfunction1.origin.params, params2, bfunction1.body, isDestructor);

        val returns = returnsFromRets + body2.resultRegister.reference

        val returnsWithoutNever =
          if (returns.size > 1 && returns.contains(Coord(Raw, Never2()))) {
            returns - Coord(Raw, Never2())
          } else {
            returns
          }
        vassert(returnsWithoutNever.nonEmpty)
        if (returnsWithoutNever.size > 1) {
          vfail("Can't infer return type because " + returnsWithoutNever.size + " types are returned:" + returnsWithoutNever.map("\n" + _))
        }
        val returnType2 = returnsWithoutNever.head

        temputs.declareFunctionReturnType(banner.toSignature, returnType2)
        val header = FunctionHeader2(functionFullName, function1.lambdaNumber, false, function1.isUserFunction, params2, returnType2, Some(function1));
        (env1, header, body2)
      }
      case Some(expectedRetCoordRune) => {
        val CoordTemplata(expectedRetCoord) =
          vassertSome(
            funcOuterEnv.getNearestTemplataWithName(
              expectedRetCoordRune, Set(TemplataLookupContext)))
        val header = FunctionHeader2(functionFullName, function1.lambdaNumber, false, function1.isUserFunction, params2, expectedRetCoord, Some(function1));
        temputs.declareFunctionReturnType(header.toSignature, expectedRetCoord)

        val funcEnvWithReturn = funcOuterEnv.copy(maybeReturnType = Some(expectedRetCoord))

        val (env6, unconvertedBody2, returnsFromRets) =
          evaluateFunctionBody(funcEnvWithReturn, temputs, bfunction1.origin.params, params2, bfunction1.body, isDestructor);

        val convertedBody2 =
          TemplataTemplar.isTypeTriviallyConvertible(temputs, unconvertedBody2.resultRegister.reference, expectedRetCoord) match {
            case (false) => {
              vfail("Function " + function1.name + "(:" + params2.mkString(", :") + ")\nreturn type:\n" + expectedRetCoord + "\ndoesn't match body's result:\n" + unconvertedBody2.resultRegister.reference)
            }
            case (true) => {
              val convertedBodyExpr2 =
                TypeTemplar.convert(env6, temputs, unconvertedBody2, expectedRetCoord);
              (Block2(List(convertedBodyExpr2)))
            }
          }

        val returns = returnsFromRets + convertedBody2.resultRegister.reference

        val returnsWithoutNever =
          if (returns.size > 1 && returns.contains(Coord(Raw, Never2()))) {
            returns - Coord(Raw, Never2())
          } else {
            returns
          }

        if (returnsWithoutNever == Set(expectedRetCoord)) {
          // Let it through, it returns the expected type.
        } else if (returnsWithoutNever == Set(Coord(Raw, Never2()))) {
          // Let it through, it returns a never but we expect something else, that's fine
        } else {
          vfail("In function " + header + ":\nExpected return type " + expectedRetCoord + " but was " + returnsWithoutNever)
        }

        (env6, header, convertedBody2)
      }
    }
  }

  private def evaluateFunctionBody(
      funcOuterEnv: FunctionEnvironment,
      temputs: TemputsBox,
      params1: List[ParameterS],
      params2: List[Parameter2],
      body1: BodyAE,
      isDestructor: Boolean):
  (FunctionEnvironment, Block2, Set[Coord]) = {
    val fate1 = funcOuterEnv.addScoutedLocals(body1.block.locals)

    val (fate2, letExprs2) =
      evaluateLets(fate1, temputs, params1, params2);

    val (fate3, postLetUnresultifiedUndestructedExpressions, returnsFromInside) =
      BlockTemplar.evaluateBlockStatements(temputs, fate2, body1.block.exprs);

    val unresultifiedUndestructedExpressions = letExprs2 ++ postLetUnresultifiedUndestructedExpressions

    val (fate6, undestructedExpressions, maybeResultLocalVariable) =
      BlockTemplar.resultifyExpressions(fate3, unresultifiedUndestructedExpressions)

    val (fate7, expressions) =
      BlockTemplar.unletUnmovedVariablesIntroducedSince(temputs, funcOuterEnv, fate6, maybeResultLocalVariable, undestructedExpressions)

    val (fate9, expressionsWithResult) =
      BlockTemplar.maybeAddUnlet(fate7, expressions, maybeResultLocalVariable)

    if (isDestructor) {
      // If it's a destructor, make sure that we've actually destroyed/moved/unlet'd
      // the parameter. For now, we'll just check if it's been moved away, but soon
      // we'll want fate to track whether it's been destroyed, and do that check instead.
      // We don't want the user to accidentally just move it somewhere, they need to
      // promise it gets destroyed.
      val destructeeName = params2.head.name
      if (!fate9.moveds.exists(_.variableName == destructeeName)) {
        vfail("Destructee wasn't moved/destroyed!");
      }
    }

    val block2 = Block2(expressionsWithResult)

    (fate9, block2, returnsFromInside)
  }

  // Produce the lets at the start of a function.
  private def evaluateLets(
      fate0: FunctionEnvironment,
      temputs: TemputsBox,
      params1: List[ParameterS],
      params2: List[Parameter2]):
  (FunctionEnvironment, List[ReferenceExpression2]) = {
    val paramLookups2 =
      params2.zipWithIndex.map({ case (p, index) => ArgLookup2(index, p.tyype) })
    val (fate1, letExprs2) =
      PatternTemplar.nonCheckingTranslateList(
        temputs, fate0, params1.map(_.pattern), paramLookups2);

    // todo: at this point, to allow for recursive calls, add a callable type to the environment
    // for everything inside the body to use

    params1.foreach({
      case ParameterS(AtomSP(Some(CaptureP(name, _)), _, _, _)) => {
        if (!fate1.variables.exists(_.id.variableName == name)) {
          vfail("wot couldnt find " + name)
        }
      }
      case _ =>
    });

    (fate1, letExprs2)
  }

}
