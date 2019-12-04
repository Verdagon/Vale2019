package net.verdagon.radonc.templar;

import net.verdagon.radonc.astronomer._
import net.verdagon.radonc.astronomer.ruletyper.{IRuleTyperEvaluatorDelegate, RuleTyperEvaluator}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.parser._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.templar.BlockTemplar.unletAll
import net.verdagon.radonc.templar.citizen.StructTemplar
import net.verdagon.radonc.templar.env._
import net.verdagon.radonc.templar.function.{DestructorTemplar, FunctionTemplar}
import net.verdagon.radonc.templar.infer.{InferSolveFailure, InferSolveSuccess}
import net.verdagon.radonc.templar.templata.TemplataTemplar
import net.verdagon.radonc.{vassert, vfail, vimpl, vwat}

import scala.collection.immutable.{List, Map, Nil, Set}

object ExpressionTemplar {
  private def evaluateList(
      temputs0: Temputs,
      fate0: FunctionEnvironment,
      expr1: List[IExpressionAE]):
      (Temputs, FunctionEnvironment, List[Expression2], Set[Coord]) = {
    expr1 match {
      case Nil => (temputs0, fate0, List(), Set())
      case first1 :: rest1 => {
        val (temputs1, fate1, first2, returnsFromFirst) =
          evaluate(temputs0, fate0, first1);
        val (temputs2, fate2, rest2, returnsFromRest) =
          evaluateList(temputs1, fate1, rest1);
        (temputs2, fate2, first2 :: rest2, returnsFromFirst ++ returnsFromRest)
      }
    }
  }

  def evaluateAndCoerceToReferenceExpressions(
      temputs0: Temputs,
      fate0: FunctionEnvironment,
      exprs1: List[IExpressionAE]):
  (Temputs, FunctionEnvironment, List[ReferenceExpression2], Set[Coord]) = {
    exprs1 match {
      case Nil => (temputs0, fate0, List(), Set())
      case first1 :: rest1 => {
        val (temputs1, fate1, first2, returnsFromFirst) =
          evaluateAndCoerceToReferenceExpression(
            temputs0, fate0, first1);
        val (temputs2, fate2, rest2, returnsFromRest) =
          evaluateAndCoerceToReferenceExpressions(
            temputs1, fate1, rest1);
        (temputs2, fate2, first2 :: rest2, returnsFromFirst ++ returnsFromRest)
      }
    }
  }

  private def evaluateLookup(
    temputs0: Temputs,
    fate0: FunctionEnvironment,
    name: String,
    borrow: Boolean):
  (FunctionEnvironment, Option[Expression2]) = {
    evaluateAddressibleLookup(temputs0, fate0, name) match {
      case Some(x) => {
        val (fate1, thing) = ExpressionTemplar.softLoad(fate0, x, borrow)
        (fate1, Some(thing))
      }
      case None => {
        fate0.getNearestTemplataWithName(name, Set(TemplataLookupContext)) match {
          case Some(IntegerTemplata(num)) => (fate0, Some(IntLiteral2(num)))
          case Some(BooleanTemplata(bool)) => (fate0, Some(BoolLiteral2(bool)))
          case None => (fate0, None)
        }
      }
    }
  }

  private def evaluateAddressibleLookup(
      temputs0: Temputs,
      fate0: FunctionEnvironment,
      name: String):
  Option[AddressExpression2] = {
    fate0.getVariable(name) match {
      case Some(alv @ AddressibleLocalVariable2(_, _, reference)) => {
        Some(LocalLookup2(alv, reference))
      }
      case Some(rlv @ ReferenceLocalVariable2(id, _, reference)) => {
        Some(LocalLookup2(rlv, reference))
      }
      case Some(AddressibleClosureVariable2(id, closuredVarsStructRef, variability, tyype)) => {
        val mutability = Templar.getMutability(temputs0, closuredVarsStructRef)
        val ownership = if (mutability == Mutable) Borrow else Share
        val closuredVarsStructRefRef = Coord(ownership, closuredVarsStructRef)
        val borrowExpr =
          ExpressionTemplar.borrowSoftLoad(
            temputs0,
            LocalLookup2(
              ReferenceLocalVariable2(VariableId2(fate0.function.lambdaNumber, "__closure"), Final, closuredVarsStructRefRef),
              closuredVarsStructRefRef))
        val lookup =
          AddressMemberLookup2(
            borrowExpr,
            name,
            id,
            tyype)
        Some(lookup)
      }
      case Some(ReferenceClosureVariable2(id, closuredVarsStructRef, _, tyype)) => {
        val mutability = Templar.getMutability(temputs0, closuredVarsStructRef)
        val ownership = if (mutability == Mutable) Borrow else Share
        val closuredVarsStructRefRef = Coord(ownership, closuredVarsStructRef)
        val borrowExpr =
          ExpressionTemplar.borrowSoftLoad(
            temputs0,
            LocalLookup2(
              ReferenceLocalVariable2(VariableId2(fate0.function.lambdaNumber, "__closure"), Final, closuredVarsStructRefRef),
              closuredVarsStructRefRef))
        val lookup =
          ReferenceMemberLookup2(
            borrowExpr,
            name,
            tyype)
        Some(lookup)
      }
      case None => None
    }
  }

  private def makeClosureStructConstructExpression(
      temputs0: Temputs,
      fate0: FunctionEnvironment,
      closureStructRef: StructRef2):
  (FunctionEnvironment, ReferenceExpression2) = {
    val closureStructDef = temputs0.lookupStruct(closureStructRef);
    // Note, this is where the unordered closuredNames set becomes ordered.
    val (fate3, lookupExpressions2) =
      closureStructDef.members.foldLeft((fate0, List[Expression2]()))({
        case ((fate1, previousExprs), StructMember2(memberName, variability, tyype)) => {
          val lookup =
            ExpressionTemplar.evaluateAddressibleLookup(temputs0, fate0, memberName) match {
              case None => vfail("Couldn't find " + memberName)
              case Some(l) => l
            }
          val (fate2, maybeDecayedExpr) =
            tyype match {
              case ReferenceMemberType2(reference) => {
                // We might have to softload an own into a borrow, but the referends
                // should at least be the same right here.
                vassert(reference.referend == lookup.resultRegister.reference.referend)
                // Closures never contain owning references.
                // If we're capturing an own, then on the inside of the closure
                // it's a borrow. See "Captured own is borrow" test for more.
                ExpressionTemplar.softLoad(fate1, lookup, borrow = true)
              }
              case AddressMemberType2(reference) => {
                vassert(reference == lookup.resultRegister.reference)
                (fate1, lookup)
              }
            }
          (fate2, previousExprs :+ maybeDecayedExpr)
        }
      });
    val ownership = if (closureStructDef.mutability == Mutable) Own else Share
    val resultPointerType = Coord(ownership, closureStructRef)
    val constructExpr2 =
      Construct2(closureStructRef, resultPointerType, lookupExpressions2)
    (fate3, constructExpr2)
  }

  def evaluateAndCoerceToReferenceExpression(
      temputs0: Temputs,
      fate0: FunctionEnvironment,
      expr1: IExpressionAE):
  (Temputs, FunctionEnvironment, ReferenceExpression2, Set[Coord]) = {
    val (temputs1, fate1, expr2, returnsFromExpr) =
      evaluate(temputs0, fate0, expr1)
    expr2 match {
      case r : ReferenceExpression2 => {
        (temputs1, fate1, r, returnsFromExpr)
      }
      case a : AddressExpression2 => {
        val (fate2, expr) = coerceToReferenceExpression(fate1, a)
        (temputs1, fate2, expr, returnsFromExpr)
      }
    }
  }

  def coerceToReferenceExpression(fate0: FunctionEnvironment, expr2: Expression2):
  (FunctionEnvironment, ReferenceExpression2) = {
    expr2 match {
      case r : ReferenceExpression2 => (fate0, r)
      case a : AddressExpression2 => softLoad(fate0, a, borrow = a.coerceToBorrow)
    }
  }

  private def evaluateExpectedAddressExpression(
      temputs0: Temputs,
      fate0: FunctionEnvironment,
      expr1: IExpressionAE):
  (Temputs, FunctionEnvironment, AddressExpression2, Set[Coord]) = {
    val (temputs1, fate1, expr2, returns) =
      evaluate(temputs0, fate0, expr1)
    expr2 match {
      case a : AddressExpression2 => (temputs1, fate1, a, returns)
      case _ : ReferenceExpression2 => vfail("Expected reference expression!")
    }
  }

  // See ClosureTests for requirements here
  def determineIfLocalIsAddressible(mutability: Mutability, variable1: LocalVariable1): Boolean = {
    if (mutability == Mutable) {
      variable1.childMutated != NotUsed || variable1.selfMoved == MaybeUsed || variable1.childMoved != NotUsed
    } else {
      variable1.childMutated != NotUsed
    }
  }


  // A user local variable is one that the user can address inside their code.
  // Users never see the names of non-user local variables, so they can't be
  // looked up.
  // Non-user local variables are reference local variables, so can't be
  // mutated from inside closures.
  def makeUserLocalVariable(
      temputs0: Temputs,
      fate0: FunctionEnvironment,
      varId: VariableId2,
      variability: Variability,
      referenceType2: Coord):
  ILocalVariable2 = {
    if (fate0.getVariable(varId.variableName).nonEmpty) {
      vfail("There's already a variable named " + varId.variableName)
    }

    val variable1 =
      fate0.scoutedLocals.find(_.varName == varId.variableName) match {
        case None => vfail("Missing local variable information from FunctionA for " + fate0.function.name + " for variable " + varId.variableName)
        case Some(v) => v
      }

    val mutable = Templar.getMutability(temputs0, referenceType2.referend)
    val addressible = determineIfLocalIsAddressible(mutable, variable1)

    val localVar =
      if (addressible) {
        AddressibleLocalVariable2(varId, variability, referenceType2)
      } else {
        ReferenceLocalVariable2(varId, variability, referenceType2)
      }
    localVar
  }

  // returns:
  // - temputs
  // - "fate", moved locals (subset of exporteds)
  // - resulting expression
  // - all the types that are returned from inside the body via ret
  private def evaluate(
      temputs0: Temputs,
      fate0: FunctionEnvironment,
      expr1: IExpressionAE):
  (Temputs, FunctionEnvironment, Expression2, Set[Coord]) = {
    expr1 match {
      case VoidAE() => (temputs0, fate0, VoidLiteral2(), Set())
      case IntLiteralAE(i) => (temputs0, fate0, IntLiteral2(i), Set())
      case BoolLiteralAE(i) => (temputs0, fate0, BoolLiteral2(i), Set())
      case StrLiteralAE(s) => (temputs0, fate0, StrLiteral2(s), Set())
      case FloatLiteralAE(f) => (temputs0, fate0, FloatLiteral2(f), Set())
      case ArgLookupAE(index) => {
        val paramCoordRune = fate0.function.params(index).pattern.coordRune
        val paramCoordTemplata = fate0.getNearestTemplataWithName(paramCoordRune, Set(TemplataLookupContext)).get
        val CoordTemplata(paramCoord) = paramCoordTemplata
        (temputs0, fate0, ArgLookup2(index, paramCoord), Set())
      }
      case FunctionCallAE(TemplateSpecifiedLookupAE(name, templateArgTemplexesS), argsPackExpr1) => {
        val (temputs3, fate3, flattenedArgsExpr2, returnsFromArgs) =
          PackTemplar.evaluate(temputs0, fate0, argsPackExpr1)
        val (temputs5, fate5, callExpr2) =
          CallTemplar.evaluatePrefixCall(
            temputs3,
            fate3,
            newGlobalFunctionGroupExpression(fate3, name),
            templateArgTemplexesS,
            flattenedArgsExpr2)
        (temputs5, fate5, callExpr2, returnsFromArgs)
      }
      case FunctionCallAE(TemplateSpecifiedLookupAE(name, templateArgTemplexesS), argsPackExpr1) => {
        val (temputs3, fate3, flattenedArgsExpr2, returnsFromArgs) =
          PackTemplar.evaluate(temputs0, fate0, argsPackExpr1)
        val (temputs5, fate5, callExpr2) =
          CallTemplar.evaluateNamedPrefixCall(temputs3, fate3, name, templateArgTemplexesS, flattenedArgsExpr2)
        (temputs5, fate5, callExpr2, returnsFromArgs)
      }
      case FunctionCallAE(GlobalLoadAE(name), argsPackExpr1) => {
        val (temputs3, fate3, flattenedArgsExpr2, returnsFromArgs) =
          PackTemplar.evaluate(temputs0, fate0, argsPackExpr1)
        val (temputs5, fate5, callExpr2) =
          CallTemplar.evaluateNamedPrefixCall(temputs3, fate3, name, List(), flattenedArgsExpr2)
        (temputs5, fate5, callExpr2, returnsFromArgs)
      }
      case FunctionCallAE(callableExpr1, argsPackExpr1) => {
        val (temputs1, fate1, undecayedCallableExpr2, returnsFromCallable) =
          evaluateAndCoerceToReferenceExpression(temputs0, fate0, callableExpr1);
        val (fate2, decayedCallableExpr2) =
          maybeSoftLoad(fate1, undecayedCallableExpr2, true)
        val (temputs3, fate3, flattenedArgsExpr2, returnsFromPack) =
          PackTemplar.evaluate(temputs1, fate2, argsPackExpr1)
        val (temputs4, fate4, functionPointerCall2) =
          CallTemplar.evaluatePrefixCall(temputs3, fate3, decayedCallableExpr2, List(), flattenedArgsExpr2)
        (temputs4, fate4, functionPointerCall2, returnsFromCallable ++ returnsFromPack)
      }

      case ExpressionLendAE(innerExpr1) => {
        val (temputs1, fate1, innerExpr2, returnsFromInner) =
          evaluateAndCoerceToReferenceExpression(temputs0, fate0, innerExpr1);
        val (temputs3, fate3, resultExpr2) =
          innerExpr2.resultRegister.underlyingReference.ownership match {
            case Borrow | Share => (temputs1, fate1, innerExpr2)
            case Own => makeTemporaryLocal(temputs1, fate1, innerExpr2)
          }
        (temputs3, fate3, resultExpr2, returnsFromInner)
      }
      case LocalLoadAE(name, borrow) => {
        val (fate2, lookupExpr1) =
          evaluateLookup(temputs0, fate0, name, borrow) match {
            case (_, None) => vfail("Couldnt find " + name)
            case (fate1, Some(x)) => (fate1, x)
          }
        (temputs0, fate2, lookupExpr1, Set())
      }
      case GlobalLoadAE(name) => {
        // Note, we don't get here if we're about to call something with this, that's handled
        // by a different case.

        // We can't use *anything* from the global environment; we're in expression context,
        // not in templata context.

        val templataFromEnv =
          fate0.getAllTemplatasWithName(name, Set(ExpressionLookupContext)) match {
            case List(BooleanTemplata(value)) => BoolLiteral2(value)
            case List(IntegerTemplata(value)) => IntLiteral2(value)
            case templatas if templatas.nonEmpty && templatas.collect({ case FunctionTemplata(_, _) => case ExternFunctionTemplata(_) => }).size == templatas.size => {
              newGlobalFunctionGroupExpression(fate0, name)
            }
            case things if things.size > 1 => {
              vfail("Found too many different things named \"" + name + "\" in env:\n" + things.map("\n" + _));
            }
            case List() => {
              println("members: " + fate0.getAllTemplatasWithName(name, Set(ExpressionLookupContext, TemplataLookupContext)))
              vfail("Couldn't find anything named \"" + name + "\" in env:\n" + fate0);
            }
          }
        (temputs0, fate0, templataFromEnv, Set())
      }
      case LocalMutateAE(name, sourceExpr1) => {
        val destinationExpr2 =
          evaluateAddressibleLookup(temputs0, fate0, name) match {
            case None => vfail("Couldnt find " + name)
            case Some(x) => x
          }
        val (temputs2, fate1, unconvertedSourceExpr2, returnsFromSource) =
          evaluateAndCoerceToReferenceExpression(temputs0, fate0, sourceExpr1)

        val (temputs3, isConvertible) =
          TemplataTemplar.isTypeConvertible(
            temputs2, unconvertedSourceExpr2.resultRegister.reference, destinationExpr2.resultRegister.reference)
        vassert(isConvertible)
        val (temputs4, convertedSourceExpr2) =
          TypeTemplar.convert(fate0, temputs3, unconvertedSourceExpr2, destinationExpr2.resultRegister.reference);

        val mutate2 = Mutate2(destinationExpr2, convertedSourceExpr2);
        (temputs4, fate1, mutate2, returnsFromSource)
      }
      case ExprMutateAE(destinationExpr1, sourceExpr1) => {
        val (temputs1, fate1, unconvertedSourceExpr2, returnsFromSource) =
          evaluateAndCoerceToReferenceExpression(temputs0, fate0, sourceExpr1)
        val (temputs3, fate2, destinationExpr2, returnsFromDestination) =
          evaluateExpectedAddressExpression(temputs1, fate1, destinationExpr1)
        val (temputs4, isConvertible) =
          TemplataTemplar.isTypeConvertible(temputs3, unconvertedSourceExpr2.resultRegister.reference, destinationExpr2.resultRegister.reference)
        if (!isConvertible) {
          vfail("In mutate, can't convert from: " + unconvertedSourceExpr2.resultRegister.reference + "\nto: " + destinationExpr2.resultRegister.reference)
        }
        val (temputs5, convertedSourceExpr2) =
          TypeTemplar.convert(fate0, temputs4, unconvertedSourceExpr2, destinationExpr2.resultRegister.reference);

        val mutate2 = Mutate2(destinationExpr2, convertedSourceExpr2);
        (temputs5, fate2, mutate2, returnsFromSource ++ returnsFromDestination)
      }
      case CheckRefCountAE(refExpr1, category, numExpr1) => {
        val (temputs1, fate1, refExpr2, returnsFromRef) =
          evaluateAndCoerceToReferenceExpression(temputs0, fate0, refExpr1);
        val (temputs2, fate2, numExpr2, returnsFromNum) =
          evaluateAndCoerceToReferenceExpression(temputs1, fate1, numExpr1);
        (temputs2, fate2, CheckRefCount2(refExpr2, category, numExpr2), returnsFromRef ++ returnsFromNum)
      }
      case TemplateSpecifiedLookupAE(name, templateArgs1) => {
        // So far, we only allow these when they're immediately called like functions
        vfail("unimplemented")
      }
      case DotCallAE(containerExpr1, indexExpr1) => {
        val (temputs1, fate1, unborrowedContainerExpr2, returnsFromContainerExpr) =
          evaluate(temputs0, fate0, containerExpr1);
        val (temputs2, fate2, containerExpr2) =
          dotBorrow(temputs1, fate1, unborrowedContainerExpr2)

        val (temputs3, fate3, indexExpr2, returnsFromIndexExpr) =
          evaluateAndCoerceToReferenceExpression(temputs2, fate2, indexExpr1);

        val exprTemplata =
          containerExpr2.resultRegister.reference.referend match {
            case at @ UnknownSizeArrayT2(_) => {
              UnknownSizeArrayLookup2(containerExpr2, at, indexExpr2)
            }
            case at @ ArraySequenceT2(_, _) => {
              ArraySequenceLookup2(
                containerExpr2,
                at,
                indexExpr2)
            }
            case at @ TupleT2(members, understruct) => {
              indexExpr2 match {
                case IntLiteral2(index) => {
                  val memberType = members(index);
                  ReferenceMemberLookup2(containerExpr2, index.toString, memberType)
                }
                case _ => vimpl("impl random access of structs' members")
              }
            }
            // later on, a map type could go here
          }
        (temputs3, fate3, exprTemplata, returnsFromContainerExpr ++ returnsFromIndexExpr)
      }
      case DotAE(containerExpr1, memberName, borrowContainer) => {
        val (temputs1, fate1, unborrowedContainerExpr2, returnsFromContainerExpr) =
          evaluate(temputs0, fate0, containerExpr1);
        val (temputs2, fate5, containerExpr2) =
          dotBorrow(temputs1, fate1, unborrowedContainerExpr2)

        val expr2 =
          containerExpr2.resultRegister.reference.referend match {
            case structRef @ StructRef2(_) => {
              temputs1.lookupStruct(structRef) match {
                case structDef : StructDefinition2 => {
                  val memberType = structDef.getMember(memberName).tyype.expectReferenceMember().reference;
                  ReferenceMemberLookup2(
                    containerExpr2,
                    memberName,
                    memberType)
                }
              }
            }
            case TupleT2(_, structRef) => {
              temputs1.lookupStruct(structRef) match {
                case structDef @ StructDefinition2(_, _, _) => {
                  val memberType = structDef.getMember(memberName).tyype.expectReferenceMember().reference;
                  ReferenceMemberLookup2(containerExpr2, memberName, memberType)
                }
              }
            }
            case as @ ArraySequenceT2(_, _) => {
              if (memberName.forall(Character.isDigit)) {
                ArraySequenceLookup2(
                  containerExpr2,
                  as,
                  IntLiteral2(memberName.toInt))
              } else {
                vfail("Sequence has no member named " + memberName)
              }
            }
            case at @ UnknownSizeArrayT2(_) => {
              if (memberName.forall(Character.isDigit)) {
                UnknownSizeArrayLookup2(
                  containerExpr2,
                  at,
                  IntLiteral2(memberName.toInt))
              } else {
                vfail("Array has no member named " + memberName)
              }
            }
            case other => {
              vfail("Can't apply ." + memberName + " to " + other)
            }
          }

        (temputs2, fate5, expr2, returnsFromContainerExpr)
      }
      case FunctionAE(function1 @ FunctionA(_, name, _, _, _, _, _, _, _, _, _, CodeBodyA(body))) => {
        val bfunction1 = BFunctionA(function1, name, body)
        val (temputs5, fate5, callExpr2) =
          evaluateClosure(temputs0, fate0, bfunction1)
        (temputs5, fate5, callExpr2, Set())
      }
      case p @ PackAE(_) => {
        // Put them all into a pack because im not quite ready yet to have packs
        // that have templatas... we'd have to make some sort of... TemplataPack thing...
        // Note from later than that: yeah we definitely need a TemplataPack btw
        val (temputs3, fate1, packExpr2, returnsFromPack) =
          PackTemplar.evaluate(temputs0, fate0, p)
        (temputs3, fate1, packExpr2, returnsFromPack)
      }
      case SequenceEAE(elements1) => {
        val (temputs1, fate1, exprs2, returnsFromElements) =
          evaluateAndCoerceToReferenceExpressions(temputs0, fate0, elements1);

        // would we need a sequence templata? probably right?
        val (temputs3, expr2) =
          SequenceTemplar.evaluate(fate0, temputs1, exprs2)
        (temputs3, fate1, expr2, returnsFromElements)
      }
      case ConstructAE(type1, argExprs1) => {
        val (temputs1, fate1, argExprs2, returnsFromArgs) =
          evaluateList(temputs0, fate0, argExprs1);

        val stuff = vfail() // this is where we do the thing

        val (temputs3, kind) = TemplataTemplar.evaluateTemplex(fate1, temputs1, stuff)
        val constructExpr2 =
          kind match {
            case KindTemplata(structRef2 @ StructRef2(_)) => {
              val structDef2 = temputs3.lookupStruct(structRef2)
              val ownership = if (structDef2.mutability == Mutable) Own else Share
              val resultPointerType = Coord(ownership, structRef2)
              Construct2(structRef2, resultPointerType, argExprs2)
            }
            case _ => vfail("wat")
          }
        (temputs3, fate1, constructExpr2, returnsFromArgs)
      }
      case ConstructArrayAE(elementCoordTemplex, sizeExpr1, generatorExpr1, arrayMutabilityP) => {
        val (temputs1, CoordTemplata(elementCoord)) = TemplataTemplar.evaluateTemplex(fate0, temputs0, elementCoordTemplex)

        val arrayMutability = Conversions.evaluateMutability(arrayMutabilityP)

        val (temputs2, fate1, sizeExpr2, returnsFromSize) =
          evaluate(temputs1, fate0, sizeExpr1);

        val (temputs3, fate2, generatorExpr2, returnsFromGenerator) =
          evaluateAndCoerceToReferenceExpression(temputs2, fate1, generatorExpr1);

        // Now, pretend we're calling it with an integer.
        // That zero should be replaced by an index in later stages.
        val (temputs4, fate3, callExpr2) =
          CallTemplar.evaluatePrefixCall(
            temputs3,
            fate2,
            generatorExpr2,
            List(),
            Placeholder2(Coord(Share, Int2())))

        val memberType2 = callExpr2.resultRegister.reference

        val (temputs5, isConvertible) =
          TemplataTemplar.isTypeTriviallyConvertible(temputs4, memberType2, elementCoord)
        if (!isConvertible) {
          vfail(memberType2 + " cant convert to " + elementCoord)
        }

        if (arrayMutability == Immutable &&
            Templar.getMutability(temputs4, elementCoord.referend) == Mutable) {
          vfail("Can't have an immutable array of mutable elements!")
        }
        val (temputs6, arrayType) = ArrayTemplar.makeUnknownSizeArrayType(fate0, temputs5, elementCoord, arrayMutability)

        val (fate4, sizeRefExpr2) = coerceToReferenceExpression(fate3, sizeExpr2)
        vassert(sizeRefExpr2.resultRegister.expectReference().reference == Coord(Share, Int2()))

        val constructExpr2 =
          ConstructArray2(
            arrayType,
            sizeRefExpr2,
            callExpr2)
        (temputs6, fate4, constructExpr2, returnsFromSize ++ returnsFromGenerator)
      }
      case LetAE(_, rulesA, typeByRune, pattern, sourceExpr1) => {
        val (temputs1, fate1, sourceExpr2, returnsFromSource) =
          evaluateAndCoerceToReferenceExpression(temputs0, fate0, sourceExpr1)

        val (temputs4, fate2, lets2) =
          PatternTemplar.nonCheckingInferAndTranslate(
            temputs1, fate1, rulesA, typeByRune, pattern, sourceExpr2)

        val resultExprBlock2 = Consecutor2(lets2)

        (temputs4, fate2, resultExprBlock2, returnsFromSource)
      }
      case IfAE(condition1, thenBody1, elseBody1) => {
        val (temputs2, fateBeforeBranch, conditionExpr2, returnsFromCondition) =
          BlockTemplar.evaluateBlock(fate0, temputs0, condition1)

        val fateForThen = fateBeforeBranch
        val FunctionEnvironment(parentEnv, function, functionFullName, entries, maybeReturnType, scoutedLocals, counterBeforeBranch, variablesBeforeBranch, _) = fateBeforeBranch

        val (temputs3, fateAfterThen, thenExpr2, returnsFromThen) =
          BlockTemplar.evaluateBlock(fateForThen, temputs2, thenBody1)
        val thenContinues = thenExpr2.resultRegister.reference.referend != Never2()
        val FunctionEnvironment(_, _, _, _, _, _, counterAfterThen, variablesAfterThen, movedsAfterThen) = fateAfterThen

        // Give the else branch the same fate the then branch got, except let the counter
        // remain higher.
        val (fateForElse, _) = fateBeforeBranch.nextCounters(counterAfterThen - counterBeforeBranch)
        val (temputs4, fateAfterElse, elseExpr2, returnsFromElse) =
          BlockTemplar.evaluateBlock(fateForElse, temputs3, elseBody1)
        val elseContinues = elseExpr2.resultRegister.reference.referend != Never2()
        val FunctionEnvironment(_, _, _, _, _, _, counterAfterElse, variablesAfterElse, movedsAfterElse) = fateAfterElse

        val ifExpr2 = If2(conditionExpr2, thenExpr2, elseExpr2)

        // We should have no new variables introduced.
        vassert(variablesBeforeBranch == variablesAfterThen)
        vassert(variablesBeforeBranch == variablesAfterElse)

        val (temputs5, fate5) =
          if (thenContinues == elseContinues) { // Both continue, or both don't
            // Each branch might have moved some things. Make sure they moved the same things.
            if (movedsAfterThen != movedsAfterElse) {
              vfail("Must move same variables from inside branches!\nFrom then branch: " + movedsAfterThen + "\nFrom else branch: " + movedsAfterElse)
            }

            val mergedFate =
              FunctionEnvironment(
                parentEnv, function, functionFullName, entries, maybeReturnType, scoutedLocals,
                counterAfterElse, // Since else took up where then left off
                variablesBeforeBranch,
                movedsAfterThen)

            // vfail("merge these function states!")
            // we used to do conditionExporteds ++ thenExporteds ++ elseExporteds

            (temputs4, mergedFate)
          } else {
            // One of them continues and the other does not.
            if (thenContinues) {
              (temputs4, fateAfterThen)
            } else if (elseContinues) {
              (temputs4, fateAfterElse)
            } else vfail()
          }
        (temputs5, fate5, ifExpr2, returnsFromCondition ++ returnsFromThen ++ returnsFromElse)
      }
      case WhileAE(condition1, body1) => {
        val (temputs2, fate2, conditionExpr2, returnsFromCondition) =
          BlockTemplar.evaluateBlock(fate0, temputs0, condition1)
        val (temputs3, fate3, bodyExpr2, returnsFromBody) =
          BlockTemplar.evaluateBlock(fate2, temputs2, body1)

        vassert(fate2.variables == fate3.variables)
        (fate2.moveds != fate3.moveds, "Don't move things from inside whiles!")

        val ifExpr2 = If2(conditionExpr2, Block2(List(bodyExpr2, BoolLiteral2(true))) , Block2(List(BoolLiteral2(false))))
        val whileExpr2 = While2(Block2(List(ifExpr2)))
        (temputs3, fate3, whileExpr2, returnsFromCondition ++ returnsFromBody)
      }
      case b @ BlockAE(_, _) => {
        val (temputs1, fate1, block2, returnsFromBlock) =
          BlockTemplar.evaluateBlock(fate0, temputs0, b);
        (temputs1, fate1, block2, returnsFromBlock)
      }
      case ArrayLengthAE(arrayExprA) => {
        val (temputs1, fate1, arrayExpr2, returnsFromArrayExpr) =
          evaluateAndCoerceToReferenceExpression(temputs0, fate0, arrayExprA);
        (temputs1, fate1, ArrayLength2(arrayExpr2), returnsFromArrayExpr)
      }
      case ReturnAE(innerExprA) => {
        val (temputs1, fate1, uncastedInnerExpr2, returnsFromInnerExpr) =
          evaluateAndCoerceToReferenceExpression(temputs0, fate0, innerExprA);

        val (temputs3, innerExpr2) =
          fate1.maybeReturnType match {
            case None => (temputs1, uncastedInnerExpr2)
            case Some(returnType) => {
              TemplataTemplar.isTypeConvertible(temputs1, uncastedInnerExpr2.resultRegister.reference, returnType) match {
                case (_, false) => vfail("Can't convert " + uncastedInnerExpr2.resultRegister.reference + " to return type " + returnType)
                case (temputs2, true) => {
                  TypeTemplar.convert(fate1, temputs2, uncastedInnerExpr2, returnType)
                }
              }
            }
          }

        val variablesToDestruct = fate1.getAllLiveLocals()
        val reversedVariablesToDestruct = variablesToDestruct.reverse

        val (fate2, resultVarNum) = fate1.nextVarCounter()
        val resultVarId = VariableId2(fate1.function.lambdaNumber, "__funcresult_" + resultVarNum)
        val resultVariable = ReferenceLocalVariable2(resultVarId, Final, innerExpr2.resultRegister.reference)
        val resultLet = LetNormal2(resultVariable, innerExpr2)
        val fate3 = fate2.addVariable(resultVariable)

        val (temputs4, fate4, destructExprs) =
          unletAll(temputs3, fate3, reversedVariablesToDestruct)

        val (fate5, getResultExpr) =
          ExpressionTemplar.unletLocal(fate4, resultVariable)

        val consecutor = Consecutor2(List(resultLet) ++ destructExprs ++ List(getResultExpr))

        val returns = returnsFromInnerExpr + innerExpr2.resultRegister.reference

        (temputs4, fate5, Return2(consecutor), returns)
      }
      case _ => {
        println(expr1)
        vfail(expr1.toString)
      }
    }
  }

  private def decaySoloPack(fate0: FunctionEnvironment, refExpr: ReferenceExpression2):
  (FunctionEnvironment, ReferenceExpression2) = {
    refExpr.resultRegister.reference.referend match {
      case PackT2(List(onlyMember), understruct) => {
        val (fate2, varNameCounter) = fate0.nextVarCounter()
        val varName = "__" + varNameCounter + "_temp"
        val varId = VariableId2(fate0.function.lambdaNumber, varName)
        val localVar = ReferenceLocalVariable2(varId, Final, onlyMember)
        val destructure = Destructure2(refExpr, understruct, List(localVar))
        val (fate3, unletExpr) = unletLocal(fate2, localVar)
        (fate3, Consecutor2(List(destructure, unletExpr)))
      }
      case _ => (fate0, refExpr)
    }
  }

  // Borrow like the . does. If it receives an owning reference, itll make a temporary.
  // If it receives an owning address, that's fine, just borrowsoftload from it.
  // Rename this someday.
  private def dotBorrow(
      temputs0: Temputs,
      fate1: FunctionEnvironment,
      undecayedUnborrowedContainerExpr2: Expression2):
  (Temputs, FunctionEnvironment, ReferenceExpression2) = {
    undecayedUnborrowedContainerExpr2 match {
      case a: AddressExpression2 => {
        (temputs0, fate1, borrowSoftLoad(temputs0, a))
      }
      case r: ReferenceExpression2 => {
        val (fate2, unborrowedContainerExpr2) = decaySoloPack(fate1, r)
        unborrowedContainerExpr2.resultRegister.reference.ownership match {
          case Own => makeTemporaryLocal(temputs0, fate2, unborrowedContainerExpr2)
          case Borrow | Share => (temputs0, fate2, unborrowedContainerExpr2)
        }
      }
    }
  }

  def makeTemporaryLocal(
      temputs0: Temputs,
      fate0: FunctionEnvironment,
      r: ReferenceExpression2):
  (Temputs, FunctionEnvironment, Defer2) = {
    val (fate1, varNameCounter) = fate0.nextVarCounter()
    val varName = "__" + varNameCounter + "_temp"
    val varId = VariableId2(fate0.function.lambdaNumber, varName)
    val rlv = ReferenceLocalVariable2(varId, Final, r.resultRegister.reference)
    val letExpr2 = LetAndLend2(rlv, r)
    val fate2 = fate1.addVariable(rlv)

    val (fate3, unlet) = ExpressionTemplar.unletLocal(fate2, rlv)
    val (temputs1, fate4, destructExpr2) =
      DestructorTemplar.drop(fate3, temputs0, unlet)

    (temputs1, fate4, Defer2(letExpr2, destructExpr2))
  }

  // Given a function1, this will give a closure (an OrdinaryClosure2 or a TemplatedClosure2)
  // returns:
  // - temputs
  // - resulting templata
  // - exported things (from let)
  // - hoistees; expressions to hoist (like initializing blocks)
  def evaluateClosure(
      temputs0: Temputs,
      fate0: FunctionEnvironment,
      function1: BFunctionA):
  (Temputs, FunctionEnvironment, ReferenceExpression2) = {

    val (temputs1, closureStructRef2) =
      FunctionTemplar.evaluateClosureStruct(temputs0, fate0, function1);
    val closureCoord =
      TemplataTemplar.pointifyReferend(temputs1, closureStructRef2, Own)

    val (fate1, constructExpr2) =
      makeClosureStructConstructExpression(temputs1, fate0, closureStructRef2)
    // The result of a constructor is always an own or a share.
    val resultExpr2 = TemplarReinterpret2(constructExpr2, closureCoord)

    // The below code was here, but i see no reason we need to put it in a temporary and lend it out.
    // shouldnt this be done automatically if we try to call the function which accepts a borrow?
//    val closureVarId = VariableId2(fate0.lambdaNumber, "__closure_" + function1.origin.lambdaNumber)
//    val closureLocalVar = ReferenceLocalVariable2(closureVarId, Final, resultExpr2.resultRegister.reference)
//    val letExpr2 = LetAndLend2(closureLocalVar, resultExpr2)
//    val (fate3, unlet2) = ExpressionTemplar.unletLocal(fate1, closureLocalVar)
//    val (temputs6, fate5, dropExpr) =
//      DestructorTemplar.drop(env, temputs3, fate3, unlet2)
//    val deferExpr2 = Defer2(letExpr2, dropExpr)
//    (temputs6, fate5, deferExpr2)

    (temputs1, fate1, resultExpr2)
  }

  def getBorrowOwnership(temputs0: Temputs, referend: Kind):
  Ownership = {
    referend match {
      case Int2() => Share
      case Bool2() => Share
      case Float2() => Share
      case Str2() => Share
      case Void2() => Raw
      case FunctionT2(_, _) => Raw
      case PackT2(_, understruct2) => {
        val mutability = Templar.getMutability(temputs0, understruct2)
        if (mutability == Mutable) Borrow else Share
      }
      case TupleT2(_, understruct2) => {
        val mutability = Templar.getMutability(temputs0, understruct2)
        if (mutability == Mutable) Borrow else Share
      }
      case ArraySequenceT2(_, RawArrayT2(_, mutability)) => {
        if (mutability == Mutable) Borrow else Share
      }
      case UnknownSizeArrayT2(array) => {
        if (array.mutability == Mutable) Borrow else Share
      }
//      case TemplatedClosure2(_, structRef, _) => {
//        val mutability = Templar.getMutability(temputs0, structRef)
//        if (mutability == Mutable) Borrow else Share
//      }
//      case OrdinaryClosure2(_, structRef, _) => {
//        val mutability = Templar.getMutability(temputs0, structRef)
//        if (mutability == Mutable) Borrow else Share
//      }
      case sr2 @ StructRef2(_) => {
        val mutability = Templar.getMutability(temputs0, sr2)
        if (mutability == Mutable) Borrow else Share
      }
      case ir2 @ InterfaceRef2(_) => {
        val mutability = Templar.getMutability(temputs0, ir2)
        if (mutability == Mutable) Borrow else Share
      }
      case OverloadSet(_, _, voidStructRef) => {
        getBorrowOwnership(temputs0, voidStructRef)
      }
    }
  }

  def maybeSoftLoad(
      fate0: FunctionEnvironment,
      expr2: Expression2,
      borrow: Boolean):
  (FunctionEnvironment, ReferenceExpression2) = {
    expr2 match {
      case e : ReferenceExpression2 => (fate0, e)
      case e : AddressExpression2 => softLoad(fate0, e, borrow)
    }
  }

  def softLoad(fate0: FunctionEnvironment, a: AddressExpression2, borrow: Boolean):
  (FunctionEnvironment, ReferenceExpression2) = {
    if (borrow) {
      val targetOwnership =
        a.resultRegister.reference.ownership match {
          case Own => Borrow
          case Borrow => Borrow // it's fine if they accidentally borrow a borrow ref
          case Share => Share
          case Raw => Raw
        }
      (fate0, SoftLoad2(a, targetOwnership))
    } else {
      a.resultRegister.reference.ownership match {
        case Own => {
          val localVar =
            a match {
              case LocalLookup2(lv, _) => lv
              case AddressMemberLookup2(_, _, _, _) => {
                vfail("Can't move out of a member!")
              }
            }
          val fate1 = fate0.markVariableMoved(localVar.id)
          (fate1, Unlet2(localVar))
        }
        case Borrow | Share | Raw => {
          (fate0, SoftLoad2(a, a.resultRegister.reference.ownership))
        }
      }
    }
  }

  def borrowSoftLoad(temputs0: Temputs, expr2: AddressExpression2):
  ReferenceExpression2 = {
    val ownership =
      getBorrowOwnership(temputs0, expr2.resultRegister.reference.referend)
    SoftLoad2(expr2, ownership)
  }

  def unletLocal(fate0: FunctionEnvironment, localVar: ILocalVariable2):
  (FunctionEnvironment, Unlet2) = {
    val fate1 = fate0.markVariableMoved(localVar.id)
    val unlet = Unlet2(localVar)
    (fate1, unlet)
  }

  private def newGlobalFunctionGroupExpression(env: IEnvironment, name: String): ReferenceExpression2 = {
    TemplarReinterpret2(
      PackTemplar.emptyPackExpression,
      Coord(
        Share,
        OverloadSet(
          env,
          name,
          Program2.emptyPackStructRef)))
  }
}
