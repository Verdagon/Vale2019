package net.verdagon.radonc.templar

import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.parser.MutableP
import net.verdagon.radonc.scout.ITemplexS
import net.verdagon.radonc.templar.ExpressionTemplar.makeTemporaryLocal
import net.verdagon.radonc.templar.OverloadTemplar.{ScoutExpectedFunctionFailure, ScoutExpectedFunctionSuccess}
import net.verdagon.radonc.templar.env.{FunctionEnvironment, IEnvironment}
import net.verdagon.radonc.templar.function.FunctionTemplar
import net.verdagon.radonc.{vassert, vfail}

import scala.collection.immutable.List

object CallTemplar {
  val CALL_FUNCTION_NAME = "__call"
  val DROP_FUNCTION_NAME = "drop"
  val INTERFACE_DESTRUCTOR_NAME = "idestructor"
  val DESTRUCTOR_NAME = "destructor"

  private def evaluateCall(
      temputs: TemputsBox,
      fate0: FunctionEnvironment,
      callableExpr: ReferenceExpression2,
      explicitlySpecifiedTemplateArgTemplexesS: List[ITemplexS],
      givenArgsExprs2: List[ReferenceExpression2]):
  (FunctionEnvironment, FunctionPointerCall2) = {
    callableExpr.resultRegister.reference.referend match {
      case Never2() | Bool2() => {
        vfail("wot " + callableExpr.resultRegister.reference.referend)
      }
      case structRef @ StructRef2(_) => {
        evaluateClosureCall(
          fate0, temputs, structRef, explicitlySpecifiedTemplateArgTemplexesS, callableExpr, givenArgsExprs2)
      }
      case interfaceRef @ InterfaceRef2(_) => {
        evaluateClosureCall(
          fate0, temputs, interfaceRef, explicitlySpecifiedTemplateArgTemplexesS, callableExpr, givenArgsExprs2)
      }
      case OverloadSet(_, functionName, _) => {
        val unconvertedArgsPointerTypes2 =
          givenArgsExprs2.map(_.resultRegister.expectReference().reference)

        // We want to get the prototype here, not the entire header, because
        // we might be in the middle of a recursive call like:
        // fn main():Int(main())

        val argsParamFilters =
          unconvertedArgsPointerTypes2.map(unconvertedArgsPointerType2 => {
            ParamFilter(unconvertedArgsPointerType2, None)
          })

        val prototype =
          OverloadTemplar.scoutExpectedFunctionForPrototype(
              fate0,
              temputs,
              functionName,
              explicitlySpecifiedTemplateArgTemplexesS,
              argsParamFilters,
              exact = false) match {
            case (seff @ ScoutExpectedFunctionFailure(_, _, _, _, _)) => {
              vfail("Couldn't find function to call!\n" + seff.toString)
            }
            case (ScoutExpectedFunctionSuccess(p)) => (p)
          }
        val argsExprs2 =
          TypeTemplar.convertExprs(
            fate0, temputs, givenArgsExprs2, prototype.functionType.paramTypes)

        CallTemplar.checkTypes(
          temputs,
          prototype.functionType.paramTypes,
          argsExprs2.map(a => a.resultRegister.reference),
          exact = true)

        (fate0, FunctionPointerCall2(FunctionLookup2(prototype), argsExprs2))
      }
      case ft @ FunctionT2(_, _) => {
        val argsExprs2 =
          TypeTemplar.convertExprs(
            fate0, temputs, givenArgsExprs2, ft.paramTypes)

        val argsPointerTypes2 = argsExprs2.map(_.resultRegister.expectReference().reference)

        val callableType = callableExpr.resultRegister.reference.referend.asInstanceOf[FunctionT2]

        checkTypes(temputs, callableType.paramTypes, argsPointerTypes2, exact = true);

        (fate0, FunctionPointerCall2(callableExpr, argsExprs2))
      }
    }
  }

  private def evaluateNamedCall(
    temputs: TemputsBox,
    fate0: FunctionEnvironment,
    functionName: String,
    explicitlySpecifiedTemplateArgTemplexesS: List[ITemplexS],
    givenArgsExprs2: List[ReferenceExpression2]):
  (FunctionEnvironment, FunctionPointerCall2) = {
    val unconvertedArgsPointerTypes2 =
      givenArgsExprs2.map(_.resultRegister.expectReference().reference)

    // We want to get the prototype here, not the entire header, because
    // we might be in the middle of a recursive call like:
    // fn main():Int(main())

    val argsParamFilters =
      unconvertedArgsPointerTypes2.map(unconvertedArgsPointerType2 => {
        ParamFilter(unconvertedArgsPointerType2, None)
      })

    val prototype =
      OverloadTemplar.scoutExpectedFunctionForPrototype(
        fate0,
        temputs,
        functionName,
        explicitlySpecifiedTemplateArgTemplexesS,
        argsParamFilters,
        exact = false) match {
        case (seff @ ScoutExpectedFunctionFailure(_, _, _, _, _)) => {
          vfail("Couldn't find function to call!\n" + seff.toString)
        }
        case (ScoutExpectedFunctionSuccess(p)) => (p)
      }
    val argsExprs2 =
      TypeTemplar.convertExprs(
        fate0, temputs, givenArgsExprs2, prototype.functionType.paramTypes)

    CallTemplar.checkTypes(
      temputs,
      prototype.functionType.paramTypes,
      argsExprs2.map(a => a.resultRegister.reference),
      exact = true)

    (fate0, FunctionPointerCall2(FunctionLookup2(prototype), argsExprs2))
  }


  // given args means, the args that the user gave, like in
  // let a = 6;
  // let f = {[a](x) print(6, x) };
  // f(4);
  // in the f(4), the given args is just 4.
  //
  // however, since f is actually a struct, it's secretly this:
  // let a = 6;
  // let f = {[a](x) print(6, x) };
  // f.__function(f.__closure, 4);
  // in that f.__function(f.__closure, 4), the given args is just 4, but the actual args is f.__closure and 4.
  // also, the given callable is f, but the actual callable is f.__function.

  private def evaluateClosureCall(
      fate0: FunctionEnvironment,
      temputs: TemputsBox,
      citizenRef: CitizenRef2,
      explicitlySpecifiedTemplateArgTemplexesS: List[ITemplexS],
      givenCallableUnborrowedExpr2: ReferenceExpression2,
      givenArgsExprs2: List[ReferenceExpression2]):
      (FunctionEnvironment, FunctionPointerCall2) = {
    val env =
      citizenRef match {
        case sr @ StructRef2(_) => temputs.envByStructRef(sr)
        case ir @ InterfaceRef2(_) => temputs.envByInterfaceRef(ir)
      }

    val argsTypes2 = givenArgsExprs2.map(_.resultRegister.reference)
    val paramFilters =
      ParamFilter(TemplataTemplar.pointifyReferend(temputs, citizenRef, Borrow), None) ::
        argsTypes2.map(argType => ParamFilter(argType, None))
    val (maybePrototype, outscoredReasonByPotentialBanner, rejectedReasonByBanner, rejectedReasonByFunctionS) =
      OverloadTemplar.scoutMaybeFunctionForPrototype(
        env, temputs, CallTemplar.CALL_FUNCTION_NAME, explicitlySpecifiedTemplateArgTemplexesS, paramFilters, false)
    val prototype2 =
      maybePrototype match {
        case None => {
          vfail(
            "Struct not callable: " + citizenRef + "\n" +
              "Outscored:\n" +
              outscoredReasonByPotentialBanner
                .map({ case (k, v) => k + ": " + v })
                .mkString("\n") +
              "\n" +
              "Rejected banners:\n" +
              rejectedReasonByBanner
                .map({ case (k, v) => k + ": " + v })
                .mkString("\n") +
              "\n" +
              "Rejected functionSs:\n" +
              rejectedReasonByFunctionS
                .map({ case (k, v) => k + ": " + v })
                .mkString("\n") +
              "\n")
        }
        case Some(p) => p
      }

    // Whether we're given a borrow or an own, the call itself will be given a borrow.
    val (fate1, givenCallableBorrowExpr2) =
      givenCallableUnborrowedExpr2.resultRegister.reference match {
        case Coord(Borrow, _) => (fate0, givenCallableUnborrowedExpr2)
        case Coord(Share, _) => (fate0, givenCallableUnborrowedExpr2)
        case Coord(Raw, _) => (fate0, givenCallableUnborrowedExpr2)
        case Coord(Own, _) => {
          ExpressionTemplar.makeTemporaryLocal(temputs, fate0, givenCallableUnborrowedExpr2)
        }
      }

    val mutability = Templar.getMutability(temputs, citizenRef)
    val ownership = if (mutability == Mutable) Borrow else Share
    val actualCallableExpr2 =
      TemplarReinterpret2(givenCallableBorrowExpr2, Coord(ownership, citizenRef))

    val actualArgsExprs2 = actualCallableExpr2 :: givenArgsExprs2

    val argTypes = actualArgsExprs2.map(_.resultRegister.reference)
    if (argTypes != prototype2.functionType.paramTypes) {
      vfail("arg param type mismatch. params: " + prototype2.functionType.paramTypes + " args: " + argTypes)
    }

    CallTemplar.checkTypes(temputs, prototype2.functionType.paramTypes, argTypes, exact = true)

    val resultingExpr2 = FunctionPointerCall2(FunctionLookup2(prototype2), actualArgsExprs2);

    (fate1, resultingExpr2)
  }


  def checkTypes(
    temputs: TemputsBox,
    params: List[Coord],
    args: List[Coord],
    exact: Boolean):
  Unit = {
    vassert(params.size == args.size)
    (params, args) match {
      case (Nil, Nil) =>
      case (paramsHead :: paramsTail, argsHead :: argsTail) => {
          if (paramsHead == argsHead) {

          } else {
            if (!exact) {
              TemplataTemplar.isTypeConvertible(temputs, argsHead, paramsHead) match {
                case (true) => {

                }
                case (false) => {
                  // do stuff here.
                  // also there is one special case here, which is when we try to hand in
                  // an owning when they just want a borrow, gotta account for that here
                  vfail("do stuff " + argsHead + " and " + paramsHead)
                }
              }
            } else {
              // do stuff here.
              // also there is one special case here, which is when we try to hand in
              // an owning when they just want a borrow, gotta account for that here
              vfail("do stuff " + argsHead + " and " + paramsHead)
            }
          }
        // It matches! Now check the rest.
        checkTypes(temputs, paramsTail, argsTail, exact)
      }
      case _ => vfail("wat")
    }
//    checkTypes(params.tail, args.tail)
//    vassert(argTypes == callableType.paramTypes, "arg param type mismatch. params: " + callableType.paramTypes + " args: " + argTypes)
  }

  def evaluatePrefixCall(
      temputs: TemputsBox,
      fate0: FunctionEnvironment,
      callableExpr2: Expression2,
      explicitlySpecifiedTemplateArgTemplexesS: List[ITemplexS],
      argsExpr2: Expression2):
  (FunctionEnvironment, FunctionPointerCall2) = {
    val (fate1, callableReferenceExpr2) =
      ExpressionTemplar.coerceToReferenceExpression(fate0, callableExpr2)
    val (fate2, argsRefExpr2) =
      ExpressionTemplar.coerceToReferenceExpression(fate1, argsExpr2);
//    val flattenedArgsExprs =
//      PackTemplar.flatten(temputs, List(argsReferenceExprs2));
    val unpackedArgsExprs =
      argsRefExpr2 match {
        case PackE2(exprs, _, _) => exprs
        case other => List(other)
      }
    val (fate3, callExpr) =
      evaluateCall(temputs, fate2, callableReferenceExpr2, explicitlySpecifiedTemplateArgTemplexesS, unpackedArgsExprs)
    (fate3, callExpr)
  }

  def evaluateNamedPrefixCall(
    temputs: TemputsBox,
    fate0: FunctionEnvironment,
    functionName: String,
    explicitlySpecifiedTemplateArgTemplexesS: List[ITemplexS],
    argsExpr2: Expression2):
  (FunctionEnvironment, FunctionPointerCall2) = {
    val (fate2, argsRefExpr2) =
      ExpressionTemplar.coerceToReferenceExpression(fate0, argsExpr2);
    //    val flattenedArgsExprs =
    //      PackTemplar.flatten(temputs, List(argsReferenceExprs2));
    val unpackedArgsExprs =
    argsRefExpr2 match {
      case PackE2(exprs, _, _) => exprs
      case other => List(other)
    }
    val (fate3, callExpr) =
      evaluateNamedCall(temputs, fate2, functionName, explicitlySpecifiedTemplateArgTemplexesS, unpackedArgsExprs)
    (fate3, callExpr)
  }
}