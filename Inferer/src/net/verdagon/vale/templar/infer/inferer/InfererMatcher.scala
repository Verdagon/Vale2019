package net.verdagon.vale.templar.infer.inferer

import net.verdagon.vale.scout._
import net.verdagon.vale.scout.patterns.{AbstractSP, AtomSP, OverrideSP}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.templar.infer._
import net.verdagon.vale.templar.templata.{Conversions, _}
import net.verdagon.vale.templar.types._
import net.verdagon.vale._

import scala.collection.immutable.List
import net.verdagon.vale.astronomer._
import net.verdagon.vale.parser.{BorrowP, OwnP, RawP, ShareP}

trait IInfererMatcherDelegate[Env, State] {
  def lookupMemberTypes(
    state: State,
    kind: Kind,
    // This is here so that the predictor can just give us however many things
    // we expect.
    expectedNumMembers: Int):
  Option[List[Coord]]

  def getMutability(state: State, kind: Kind): Mutability

  def getAncestorInterfaceDistance(
    temputs: State,
    descendantCitizenRef: CitizenRef2,
    ancestorInterfaceRef: InterfaceRef2):
  (Option[Int])

  def citizenIsFromTemplate(state: State, citizen: CitizenRef2, template: ITemplata): Boolean

  def getAncestorInterfaces(temputs: State, descendantCitizenRef: CitizenRef2): Set[InterfaceRef2]

  def structIsClosure(state: State, structRef: StructRef2): Boolean

  def getSimpleInterfaceMethod(state: State, interfaceRef: InterfaceRef2): Prototype2
}

class InfererMatcher[Env, State](
    templataTemplar: TemplataTemplarInner[Env, State],
    equator: InfererEquator[Env, State],
    evaluate: (Env, State, InferencesBox, IRulexAR) => (IInferEvaluateResult[ITemplata]),
    delegate: IInfererMatcherDelegate[Env, State]) {

  private[infer] def matchTemplataAgainstRuneSP(
    env: Env,
    state: State,
    inferences: InferencesBox,
    instance: ITemplata,
    rune: String,
    expectedType: ITemplataType,
    ):
  (IInferMatchResult) = {

    inferences.templatasByRune.get(rune) match {
      case None => {
        inferences.addConclusion(rune, instance)
        (InferMatchSuccess(true))
      }
      case Some(alreadyInferredTemplata) => {
        val equal =
          equator.equals(state, instance, alreadyInferredTemplata, expectedType)
        if (equal) {
          (InferMatchSuccess(true))
        } else {
          (InferMatchConflict(inferences.inferences, s"Disagreement about templata #${rune}:\n${alreadyInferredTemplata}\n${instance}", List()))
        }
      }
    }
  }

  private[infer] def matchReference2AgainstRuneSP(
    env: Env,
    state: State,
    inferences: InferencesBox,
    instance: Coord,
    coordRune: String):
  (IInferMatchResult) = {

    inferences.templatasByRune.get(coordRune) match {
      case None => {
        inferences.addConclusion(coordRune, CoordTemplata(instance))
        (InferMatchSuccess(true))
      }
      case Some(CoordTemplata(alreadyInferredCoord)) => {
        if (instance == alreadyInferredCoord) {
          (InferMatchSuccess(true))
        } else {
          (InferMatchConflict(inferences.inferences, s"Disagreement about ref #${coordRune}:\n${CoordTemplata(alreadyInferredCoord)}\n${instance}", List()))
        }
      }
    }
  }

  private[infer] def matchReferend2AgainstRuneSP(
    env: Env,
    state: State,
    inferences: InferencesBox,
    instance: Kind,
    kindRune: String):
  (IInferMatchResult) = {
    inferences.templatasByRune.get(kindRune) match {
      case None => {
        inferences.addConclusion(kindRune, KindTemplata(instance))
        (InferMatchSuccess(true))
      }
      case Some(KindTemplata(alreadyInferredKind)) => {
        if (instance == alreadyInferredKind) {
          (InferMatchSuccess(true))
        } else {
          (InferMatchConflict(inferences.inferences, s"Disagreement about kind #${alreadyInferredKind}:\n${KindTemplata(alreadyInferredKind)}\n${instance}", List()))
        }
      }
    }
  }

  private[infer] def matchReference2AgainstDestructure(
    env: Env,
    state: State,
    inferences: InferencesBox,
    instance: Coord,
    parts: List[Option[AtomSP]]):
  (IInferMatchResult) = {

    val structMemberTypes =
      delegate.lookupMemberTypes(state, instance.referend, expectedNumMembers = parts.size) match {
        case None => vfail("this thing cant be destructured, has no member types!")
        case Some(x) => x
      }

    val destructuresDeeplySatisfied =
      structMemberTypes.zip(parts).foldLeft((true))({
        case ((deeplySatisfiedSoFar), (_, None)) => {
          // The part is None; there's just an _ in this spot of the destructure.
          // Nothing to do here.
          (deeplySatisfiedSoFar)
        }
        case ((deeplySatisfiedSoFar), (structMemberType, Some(part))) => {
          val paramFilter = ParamFilter(structMemberType, None)
          matchParamFilterAgainstAtomSP(env, state, inferences, paramFilter, part) match {
            case (imc @ InferMatchConflict(_, _, _)) => return (imc)
            case (InferMatchSuccess(deeplySatisfied)) => (deeplySatisfiedSoFar && deeplySatisfied)
          }
        }
      })

    (InferMatchSuccess(destructuresDeeplySatisfied))
  }

  private[infer] def matchParamFilterAgainstAtomSP(
      env: Env,
      state: State,
      inferences: InferencesBox,
      instance: ParamFilter,
      rule: AtomSP):
  (IInferMatchResult) = {
    val coordDeeplySatisfied =
      matchReference2AgainstRuneSP(env, state, inferences, instance.tyype, rule.coordRune) match {
        case (imc @ InferMatchConflict(_, _, _)) => return (imc)
        case (InferMatchSuccess(ds)) => (ds)
      }

    val destructureDeeplySatisfied =
      rule.destructure match {
        case None => (true)
        case Some(parts) => {
          matchReference2AgainstDestructure(env, state, inferences, instance.tyype, parts) match {
            case (imc @ InferMatchConflict(_, _, _)) => return (imc)
            case (InferMatchSuccess(ds)) => (ds)
          }
        }
      }

    val virtualityDeeplySatisfied =
      ((instance.virtuality, rule.virtuality) match {
        case (None, _) => (true)
        case (Some(Abstract2), Some(AbstractSP)) => (true)
        case (Some(Abstract2), _) => return (InferMatchConflict(inferences.inferences, s"ParamFilter virtuality didn't match rule:\n${instance.virtuality}\n${rule.virtuality}", List()))
        case (Some(Override2(instanceSuperInterfaceRef2)), Some(OverrideSP(kindRune))) => {
          matchReferend2AgainstRuneSP(env, state, inferences, instanceSuperInterfaceRef2, kindRune) match {
            case (imc @ InferMatchConflict(_, _, _)) => return (imc)
            case (InferMatchSuccess(ds)) => (ds)
          }
        }
        case (Some(Override2(_)), _) => return (InferMatchConflict(inferences.inferences, s"ParamFilter virtuality didn't match rule:\n${instance.virtuality}\n${rule.virtuality}", List()))
      })

    (InferMatchSuccess(coordDeeplySatisfied && destructureDeeplySatisfied && virtualityDeeplySatisfied))
  }

  private[infer] def matchCitizenAgainstCallTT(
    env: Env,
    state: State,
    inferences: InferencesBox,
    call: CallAT,
    actualCitizen: CitizenRef2):
  IInferMatchResult = {
    evaluate(env, state, inferences, TemplexAR(call.template)) match {
      case (iec @ InferEvaluateConflict(_, _, _)) => return (InferMatchConflict(inferences.inferences, "Couldn't evaluate template!", List(iec)))
      case (InferEvaluateUnknown(_)) => {
        vcurious() // Can this ever happen? If it does, is the below conflict appropriate?
        (InferMatchConflict(inferences.inferences, "Couldn't figure out template!", List()))
      }
      case (InferEvaluateSuccess(callTemplateTemplata, templateDeeplySatisfied)) => {
        // debt: TEST THIS!

        if (delegate.citizenIsFromTemplate(state, actualCitizen, callTemplateTemplata)) {
          val expectedArgs = call.args
          if (actualCitizen.fullName.steps.size > 1) {
            vimpl()
          }
          val actualArgs = actualCitizen.fullName.steps.last.templateArgs.get

          // Check to see that the actual template args match the expected template args
          val argsDeeplySatisfied =
            expectedArgs.zip(actualArgs).foldLeft((true))({
              case ((deeplySatisfiedSoFar), (expectedArg, actualArg)) => {
                matchTemplataAgainstTemplexAR(env, state, inferences, actualArg, expectedArg) match {
                  case (imc @ InferMatchConflict(_, _, _)) => return (imc)
                  case (InferMatchSuccess(deeplySatisfied)) => (deeplySatisfiedSoFar && deeplySatisfied)
                }
              }
            })
          // If the function is the same, and the args are the same... it's the same.
          InferMatchSuccess(templateDeeplySatisfied && argsDeeplySatisfied)
        } else {
          return InferMatchConflict(inferences.inferences, "Given citizen didn't come from expected template!\nCitizen: " + actualCitizen + "\nTemplate: " + callTemplateTemplata, List())
        }
      }
    }
  }

  private[infer] def matchArrayAgainstCallTT(
    env: Env,
    state: State,
    inferences: InferencesBox,
    expectedTemplate: ITemplexA,
    expectedArgs: List[ITemplexA],
    actualArgs: List[ITemplata]):
  (IInferMatchResult) = {
    // Check to see that the actual template matches the expected template
    val templateDeeplySatisfied =
      matchTemplataAgainstTemplexAR(env, state, inferences, ArrayTemplateTemplata(), expectedTemplate) match {
        case (imc @ InferMatchConflict(_, _, _)) => return (imc)
        case (InferMatchSuccess(ds)) => (ds)
      }
    // Check to see that the actual template args match the expected template args
    val argsDeeplySatisfied =
      expectedArgs.zip(actualArgs).foldLeft((true))({
        case ((deeplySatisfiedSoFar), (expectedArg, actualArg)) => {
          matchTemplataAgainstTemplexAR(env, state, inferences, actualArg, expectedArg) match {
            case (imc @ InferMatchConflict(_, _, _)) => return (imc)
            case (InferMatchSuccess(deeplySatisfied)) => (deeplySatisfiedSoFar && deeplySatisfied)
          }
        }
      })
    // If the function is the same, and the args are the same... it's the same.
    (InferMatchSuccess(templateDeeplySatisfied && argsDeeplySatisfied))
  }

  private[infer] def matchTemplataAgainstTemplexAR(
      env: Env,
      state: State,
      inferences: InferencesBox,
      instance: ITemplata,
      rule: ITemplexA):
  (IInferMatchResult) = {
    (rule, instance) match {
      case (AnonymousRuneAT(expectedType), _) => {
        vassert(instance.tyype == expectedType)
        (InferMatchSuccess(true))
      }
      case (IntAT(expectedValue), IntegerTemplata(actualValue))
          if actualValue == expectedValue => {
        (InferMatchSuccess(true))
      }
      case (BoolAT(expectedValue), BooleanTemplata(actualValue))
          if actualValue == expectedValue => {
        (InferMatchSuccess(true))
      }
      case (OwnershipAT(expectedOwnership), OwnershipTemplata(actualOwnership)) => {
        if (actualOwnership == Conversions.evaluateOwnership(expectedOwnership)) {
          return (InferMatchSuccess(true))
        } else if (actualOwnership == Share) {
          // Share is compatible with anything except raw
          if (expectedOwnership != RawP) {
            return (InferMatchSuccess(true))
          }
        } else if (actualOwnership == Raw) {
          // Raw is compatible with anything except share
          if (expectedOwnership != ShareP) {
            return (InferMatchSuccess(true))
          }
        }
        return (InferMatchConflict(inferences.inferences, s"Supplied ${actualOwnership} doesn't match expected ${expectedOwnership}", List()))
      }
      case (MutabilityAT(expectedMutability), MutabilityTemplata(actualMutability)) => {
        if (actualMutability == Conversions.evaluateMutability(expectedMutability)) {
          (InferMatchSuccess(true))
        } else {
          return (InferMatchConflict(inferences.inferences, s"Supplied ${actualMutability} doesn't match expected ${expectedMutability}", List()))
        }
      }
      case (PermissionAT(expectedPermission), PermissionTemplata(actualPermission)) => {
        if (actualPermission == Conversions.evaluatePermission(expectedPermission)) {
          (InferMatchSuccess(true))
        } else {
          return (InferMatchConflict(inferences.inferences, s"Supplied ${actualPermission} doesn't match expected ${expectedPermission}", List()))
        }
      }
      case (LocationAT(expectedLocation), LocationTemplata(actualLocation)) => {
        if (actualLocation == Conversions.evaluateLocation(expectedLocation)) {
          (InferMatchSuccess(true))
        } else {
          return (InferMatchConflict(inferences.inferences, s"Supplied ${actualLocation} doesn't match expected ${expectedLocation}", List()))
        }
      }
      case (VariabilityAT(expectedVariability), VariabilityTemplata(actualVariability)) => {
        if (actualVariability == Conversions.evaluateVariability(expectedVariability)) {
          (InferMatchSuccess(true))
        } else {
          return (InferMatchConflict(inferences.inferences, s"Supplied ${actualVariability} doesn't match expected ${expectedVariability}", List()))
        }
      }
      case (NameAT(name, expectedType), actualTemplata) => {
        val expectedTemplata = templataTemplar.lookupTemplata(env, state, name, expectedType)
        if (actualTemplata != expectedTemplata) {
          // Right here, thought about checking for subtypes, but I don't think we should.
          // For example, let's say we have this impl:
          //   impl IMiddleInterface for ITopInterface;
          // and a struct MyStruct that implements IMiddleInterface.
          //
          // If we search for all superinterfaces of IMiddleInterface, we'll be testing
          // IMiddleInterface against IMiddleInterface, and itll correctly tell us that
          // yes, it matches.
          // If, however, we search for all superinterfaces of MyStruct, we'll be testing
          // against that impl and ask if MyStruct matches that IMiddleInterface. We want
          // it to say "no, it doesn't match." here.
          //
          // If we decide to check for subtypes here, it will do the incorrect thing in
          // that latter case. So, we don't check for subtypes here, just strict equality.
          return (InferMatchConflict(inferences.inferences, s"Supplied templata doesn't match '${name}':\n'${name}' in environment:${expectedTemplata}\nActual:${actualTemplata}", List()))
        }
        (InferMatchSuccess(true))
      }
      case (RuneAT(rune, expectedType), actualTemplata) => {
        if (actualTemplata.tyype != expectedType) {
          return (InferMatchConflict(inferences.inferences, s"Doesn't match type! Expected ${expectedType} but received ${actualTemplata}", List()))
        }
        matchTemplataAgainstRuneSP(env, state, inferences, actualTemplata, rune, expectedType) match {
          case imc @ InferMatchConflict(_, _, _) => return imc
          case ims @ InferMatchSuccess(_) => ims
        }
      }
      case (ct @ CallAT(_, _, _), CoordTemplata(Coord(_, structRef @ StructRef2(_)))) => {
        vassert(instance.tyype == ct.resultType)

//        if (delegate.structIsClosure(state, structRef)) {
//          // If it's a closure, see if we can conform it to the receiving interface.
//
//          // We can make this smarter later, but for now, require that we have enough information
//          // up-front to completely know what the receiving thing is.
//          evaluate(env, state, inferences, TemplexAR(ct)) match {
//            case InferEvaluateSuccess(templata, deeplySatisfied) => {
//              vassert(deeplySatisfied)
//              templata match {
//                case CoordTemplata(coord) => vimpl()
//                case _ => vwat()
//              }
//            }
//            case InferEvaluateUnknown(_) => {
//              vimpl("Shortcalling inferring not implemented yet!")
//            }
//            case iec @ InferEvaluateConflict(_, _, _) => InferMatchConflict(inferences.inferences, "Conflict in shortcall", List(iec))
//          }
//        } else {
          // If its not a closure, then there's nothing special to do here.

          matchCitizenAgainstCallTT(env, state, inferences, ct, structRef)
//        }


        // this will get us... the FunctionA for the interface.
        //              val interfaceMethod =
        //                delegate.getSimpleInterfaceMethod(state, callTemplateTemplata)
        // Now let's make a little sub-world to try and figure out its runes.
        // We know one of its rune parameters so we can supply the int there...
        // Then we'll realize we know all the function parameters, but not the return
        // type, so we'll try evaluating the function.
        // We'll then get the return type of the function, and then set the rune.
        // Then we'll know the full IFunction1, and can proceed to glory.
      }
      case (ct @ CallAT(_, _, _), CoordTemplata(Coord(_, cit @ InterfaceRef2(_)))) => {
        vassert(instance.tyype == ct.resultType)
        matchCitizenAgainstCallTT(env, state, inferences, ct, cit)
      }
      case (ct @ CallAT(_, _, _), KindTemplata(structRef @ StructRef2(_))) => {
        vassert(instance.tyype == ct.resultType)

//        if (delegate.structIsClosure(state, structRef)) {
//          // If it's a closure, see if we can conform it to the receiving interface.
//
//          // We can make this smarter later, but for now, require that we have enough information
//          // up-front to completely know what the receiving thing is.
//          evaluate(env, state, inferences, TemplexAR(ct)) match {
//            case InferEvaluateSuccess(templata, deeplySatisfied) => {
//              vassert(deeplySatisfied)
//              templata match {
//                case CoordTemplata(coord) => vimpl()
//                case _ => vwat()
//              }
//            }
//            case InferEvaluateUnknown(_) => {
//              vimpl("Shortcalling inferring not implemented yet!")
//            }
//            case iec @ InferEvaluateConflict(_, _, _) => InferMatchConflict(inferences.inferences, "Conflict in shortcall", List(iec))
//          }
//        } else {
          // If its not a closure, then there's nothing special to do here.

          matchCitizenAgainstCallTT(env, state, inferences, ct, structRef)
//        }


        // this will get us... the FunctionA for the interface.
        //              val interfaceMethod =
        //                delegate.getSimpleInterfaceMethod(state, callTemplateTemplata)
        // Now let's make a little sub-world to try and figure out its runes.
        // We know one of its rune parameters so we can supply the int there...
        // Then we'll realize we know all the function parameters, but not the return
        // type, so we'll try evaluating the function.
        // We'll then get the return type of the function, and then set the rune.
        // Then we'll know the full IFunction1, and can proceed to glory.
      }
      case (ct @ CallAT(_, _, _), KindTemplata(cit @ InterfaceRef2(_))) => {
        vassert(instance.tyype == ct.resultType)
        matchCitizenAgainstCallTT(env, state, inferences, ct, cit)
      }
      case (CallAT(expectedTemplate, expectedArgs, resultType), KindTemplata(UnknownSizeArrayT2(RawArrayT2(elementArg,mutability)))) => {
        vassert(instance.tyype == resultType)
        matchArrayAgainstCallTT(
          env, state, inferences, expectedTemplate, expectedArgs, List(MutabilityTemplata(mutability), CoordTemplata(elementArg)))
      }
      case (CallAT(_, _, _), KindTemplata(ArraySequenceT2(_, RawArrayT2(_, _)))) => {
        return (InferMatchConflict(inferences.inferences, "Can't match array sequence against anything, no such rule exists", List()))
      }
      case (CallAT(_, _, _), CoordTemplata(Coord(_, ArraySequenceT2(_, _)))) => {
        return (InferMatchConflict(inferences.inferences, "Can't match array sequence against anything, no such rule exists", List()))
      }
      case (CallAT(expectedTemplate, expectedArgs, resultType), CoordTemplata(Coord(instanceOwnership, UnknownSizeArrayT2(RawArrayT2(elementArg,mutability))))) => {
        vassert(instance.tyype == resultType)
        matchArrayAgainstCallTT(
          env, state, inferences, expectedTemplate, expectedArgs, List(MutabilityTemplata(mutability), CoordTemplata(elementArg)))
      }
      case (PrototypeAT(_, _, _), _) => {
        vfail("what even is this")
      }
      case (PackAT(expectedMembers, _), KindTemplata(PackT2(actualMembers, _))) => {
        val membersDeeplySatisfied =
          expectedMembers.zip(actualMembers).foldLeft((true))({
            case ((deeplySatisfiedSoFar), (expectedMember, actualMember)) => {
              matchTemplataAgainstTemplexAR(env, state, inferences, CoordTemplata(actualMember), expectedMember) match {
                case (imc @ InferMatchConflict(_, _, _)) => return (imc)
                case (InferMatchSuccess(deeplySatisfied)) => (deeplySatisfiedSoFar && deeplySatisfied)
              }
            }
          })
        (InferMatchSuccess(membersDeeplySatisfied))
      }
      case (RepeaterSequenceAT(mutabilityTemplex, sizeTemplex, elementTemplex, resultType), CoordTemplata(Coord(ownership, ArraySequenceT2(size, RawArrayT2(elementCoord, mutability))))) => {
        vassert(resultType == CoordTemplataType)
        vcurious(ownership == Share || ownership == Own)

        val mutabilityDeeplySatisfied =
          matchTemplataAgainstTemplexAR(env, state, inferences, MutabilityTemplata(mutability), mutabilityTemplex) match {
            case (imc @ InferMatchConflict(_, _, _)) => return (imc)
            case (InferMatchSuccess(deeplySatisfied)) => (deeplySatisfied)
          }

        val sizeDeeplySatisfied =
          matchTemplataAgainstTemplexAR(env, state, inferences, IntegerTemplata(size), sizeTemplex) match {
            case (imc @ InferMatchConflict(_, _, _)) => return (imc)
            case (InferMatchSuccess(deeplySatisfied)) => (deeplySatisfied)
          }

        val elementDeeplySatisfied =
          matchTemplataAgainstTemplexAR(env, state, inferences, CoordTemplata(elementCoord), elementTemplex) match {
            case (imc @ InferMatchConflict(_, _, _)) => return (imc)
            case (InferMatchSuccess(deeplySatisfied)) => (deeplySatisfied)
          }

        val deeplySatisfied = mutabilityDeeplySatisfied && sizeDeeplySatisfied && elementDeeplySatisfied
        (InferMatchSuccess(deeplySatisfied))
      }
      case (RepeaterSequenceAT(mutabilityTemplex, sizeTemplex, elementTemplex, resultType), KindTemplata(ArraySequenceT2(size, RawArrayT2(elementCoord, mutability)))) => {
        vassert(resultType == KindTemplataType)

        val mutabilityDeeplySatisfied =
          matchTemplataAgainstTemplexAR(env, state, inferences, MutabilityTemplata(mutability), mutabilityTemplex) match {
            case (imc @ InferMatchConflict(_, _, _)) => return (imc)
            case (InferMatchSuccess(deeplySatisfied)) => (deeplySatisfied)
          }

        val sizeDeeplySatisfied =
          matchTemplataAgainstTemplexAR(env, state, inferences, IntegerTemplata(size), sizeTemplex) match {
            case (imc @ InferMatchConflict(_, _, _)) => return (imc)
            case (InferMatchSuccess(deeplySatisfied)) => (deeplySatisfied)
          }

        val elementDeeplySatisfied =
          matchTemplataAgainstTemplexAR(env, state, inferences, CoordTemplata(elementCoord), elementTemplex) match {
            case (imc @ InferMatchConflict(_, _, _)) => return (imc)
            case (InferMatchSuccess(deeplySatisfied)) => (deeplySatisfied)
          }

        val deeplySatisfied = mutabilityDeeplySatisfied && sizeDeeplySatisfied && elementDeeplySatisfied
        (InferMatchSuccess(deeplySatisfied))
      }
      case (RepeaterSequenceAT(_, _, _, _), KindTemplata(otherKind)) => {
        (InferMatchConflict(inferences.inferences, "Expected repeater sequence, was: " + otherKind, List()))
      }
      case (RepeaterSequenceAT(_, _, _, _), CoordTemplata(otherCoord)) => {
        (InferMatchConflict(inferences.inferences, "Expected repeater sequence, was: " + otherCoord, List()))
      }
      case (OwnershipAT(ownershipP), OwnershipTemplata(ownershipT)) => {
        if (ownershipT == Share) {
          // Doesn't matter what the ownership rule was, ownership doesnt apply to Share.
          (InferMatchSuccess(true))
        } else if (ownershipT == Conversions.evaluateOwnership(ownershipP)) {
          (InferMatchSuccess(true))
        } else {
          (InferMatchConflict(inferences.inferences, s"Ownerships don't match: ${ownershipP} and ${ownershipT}", List()))
        }
      }
      case (OwnershippedAT(expectedOwnership, innerCoordTemplex), CoordTemplata(Coord(instanceOwnership, instanceKind))) => {
        val compatible =
          (instanceOwnership, expectedOwnership) match {
            case (Own, OwnP) => true
            case (Own, BorrowP) => false
            case (Own, ShareP) => false
            case (Own, RawP) => false

            case (Borrow, OwnP) => false
            case (Borrow, BorrowP) => true
            case (Borrow, ShareP) => false
            case (Borrow, RawP) => false

            case (Raw, OwnP) => true
            case (Raw, BorrowP) => true
            case (Raw, ShareP) => false
            case (Raw, RawP) => true

            case (Share, OwnP) => true
            case (Share, BorrowP) => true
            case (Share, ShareP) => true
            case (Share, RawP) => false
          }
        if (compatible) {
          // The incoming thing is a borrow, and we expect a borrow, so send a regular own into the inner rule matcher.
          if (instanceOwnership == Borrow && expectedOwnership == BorrowP) {
            matchTemplataAgainstTemplexAR(env, state, inferences, CoordTemplata(Coord(Own, instanceKind)), innerCoordTemplex)
          } else {
            matchTemplataAgainstTemplexAR(env, state, inferences, CoordTemplata(Coord(instanceOwnership, instanceKind)), innerCoordTemplex)
          }
        } else {
          (InferMatchConflict(inferences.inferences, s"Couldn't match incoming ${instanceOwnership} against expected ${expectedOwnership}", List()))
        }
      }
      case _ => vfail("Can't match rule " + rule + " against instance " + instance)
    }
  }

  private[infer] def matchTemplataAgainstRulexTR(
    env: Env,
    state: State,
    inferences: InferencesBox,
    instance: ITemplata,
    irule: IRulexAR):
  (IInferMatchResult) = {
    irule match {
      case rule @ EqualsAR(_, _) => {
        matchTemplataAgainstEqualsAR(env, state, inferences, instance, rule)
      }
      case rule @ IsaAR(_, _) => {
        matchTemplataAgainstIsaAR(env, state, inferences, instance, rule)
      }
      case rule @ OrAR(_) => {
        matchTemplataAgainstOrAR(env, state, inferences, instance, rule)
      }
      case rule @ ComponentsAR(_, _) => {
        matchTemplataAgainstComponentsAR(env, state, inferences, instance, rule)
      }
      case TemplexAR(itemplexTT) => {
        matchTemplataAgainstTemplexAR(env, state, inferences, instance, itemplexTT)
      }
      case rule @ CallAR(_, _, _) => {
        matchTemplataAgainstCallAR(env, state, inferences, instance, rule)
      }
    }
  }

  // debt: rename from instance
  private[infer] def matchTemplataAgainstCallAR(
    env: Env,
    state: State,
    inferences: InferencesBox,
    instance: ITemplata,
    rule: CallAR):
  (IInferMatchResult) = {
    vassert(instance.tyype == rule.resultType)

    // We don't match into the argRules here, see MDMIA.
    // But we can't just *not* match them, and evaluating could return unknown, in which case we
    // don't know what to do.
    // For now, we'll only allow calls like toRef that are 1<1>, and so can be matched.

    val CallAR(name, args, resultType) = rule

    if (instance.tyype != resultType) {
      return (InferMatchConflict(inferences.inferences, "Call result expected type " + resultType + ", but was " + instance, List()))
    }

    name match {
      case "toRef" => {
        val List(kindRule) = args
        instance match {
          case CoordTemplata(Coord(instanceOwnership, instanceKind)) => {
            val defaultOwnershipForKind =
              if (delegate.getMutability(state, instanceKind) == Mutable) Own else Share
            if (instanceOwnership != defaultOwnershipForKind) {
              return (InferMatchConflict(inferences.inferences, "Coord matching into toRef doesn't have default ownership: " + instanceOwnership, List()))
            }
            matchTemplataAgainstRulexTR(env, state, inferences, KindTemplata(instanceKind), kindRule)
          }
          case _ => return (InferMatchConflict(inferences.inferences, "Bad arguments to toRef: " + args, List()))
        }
      }
      case "passThroughIfConcrete" => {
        val List(kindRule) = args
        instance match {
          case KindTemplata(StructRef2(_) | PackT2(_, _) | TupleT2(_, _) | ArraySequenceT2(_, _) | UnknownSizeArrayT2(_)) => {
            matchTemplataAgainstRulexTR(env, state, inferences, instance, kindRule)
          }
          case _ => return (InferMatchConflict(inferences.inferences, "Bad arguments to passThroughIfConcrete: " + args, List()))
        }
      }
      case "passThroughIfInterface" => {
        val List(kindRule) = args
        instance match {
          case KindTemplata(InterfaceRef2(_)) => {
            matchTemplataAgainstRulexTR(env, state, inferences, instance, kindRule)
          }
          case _ => return (InferMatchConflict(inferences.inferences, "Bad arguments to passThroughIfInterface: " + args, List()))
        }
      }
      case "passThroughIfStruct" => {
        val List(kindRule) = args
        instance match {
          case KindTemplata(StructRef2(_)) => {
            matchTemplataAgainstRulexTR(env, state, inferences, instance, kindRule)
          }
          case _ => return (InferMatchConflict(inferences.inferences, "Bad arguments to passThroughIfStruct: " + args, List()))
        }
      }
    }
  }

  private[infer] def matchTemplataAgainstComponentsAR(
    env: Env,
    state: State,
    inferences: InferencesBox,
    instance: ITemplata,
    rule: ComponentsAR):
  (IInferMatchResult) = {
    val ComponentsAR(tyype, components) = rule

    if (!equator.templataMatchesType(instance, tyype)) {
      return (InferMatchConflict(inferences.inferences, s"Supplied templata isn't the right type! Type: ${rule.tyype} but gave: ${instance}", List()))
    }

    instance match {
      case KindTemplata(actualReferend) => {
        components match {
          case List(mutabilityRule) => {
            val actualMutability = delegate.getMutability(state, actualReferend)
            matchTemplataAgainstRulexTR(
              env, state, inferences, MutabilityTemplata(actualMutability), mutabilityRule)
          }
          case _ => vfail("Wrong number of components for kind")
        }
      }
      case CoordTemplata(actualReference) => {
        components match {
          case List(ownershipRule, kindRule) => {
            val actualOwnership = OwnershipTemplata(actualReference.ownership)
            val ownershipDeeplySatisfied =
              matchTemplataAgainstRulexTR(env, state, inferences, actualOwnership, ownershipRule) match {
                case (imc @ InferMatchConflict(_, _, _)) => return (imc)
                case (InferMatchSuccess(ods)) => (ods)
              }
            val actualKind = KindTemplata(actualReference.referend)
            val kindDeeplySatisfied =
              matchTemplataAgainstRulexTR(env, state, inferences, actualKind, kindRule) match {
                case (imc @ InferMatchConflict(_, _, _)) => return (imc)
                case (InferMatchSuccess(kds)) => (kds)
              }
            (InferMatchSuccess(ownershipDeeplySatisfied && kindDeeplySatisfied))
          }
          case _ => vfail("Wrong number of components for kind")
        }
      }
    }
  }

  private[infer] def matchTemplataAgainstEqualsAR(
    env: Env,
    state: State,
    inferences: InferencesBox,
    instance: ITemplata,
    rule: EqualsAR):
  (IInferMatchResult) = {
    val EqualsAR(left, right) = rule

    matchTemplataAgainstRulexTR(env, state, inferences, instance, left) match {
      case (imc @ InferMatchConflict(_, _, _)) => (imc)
      case (InferMatchSuccess(leftDeeplySatisfied)) => {
        matchTemplataAgainstRulexTR(env, state, inferences, instance, right) match {
          case (imc @ InferMatchConflict(_, _, _)) => (imc)
          case (InferMatchSuccess(rightDeeplySatisfied)) => {
            (InferMatchSuccess(leftDeeplySatisfied && rightDeeplySatisfied))
          }
        }
      }
    }
  }

  private[infer] def matchTemplataAgainstIsaAR(
    env: Env,
    state: State,
    inferences: InferencesBox,
    subTemplata: ITemplata,
    rule: IsaAR):
  (IInferMatchResult) = {
    val IsaAR(left, right) = rule

    matchTemplataAgainstRulexTR(env, state, inferences, subTemplata, left) match {
      case (imc @ InferMatchConflict(_, _, _)) => (imc)
      case (InferMatchSuccess(subDeeplySatisfied)) => {
        evaluate(env, state, inferences, right) match {
          case (iec @ InferEvaluateConflict(_, _, _)) => return (InferMatchConflict(inferences.inferences, "Couldn't evaluate concept!", List(iec)))
          case (InferEvaluateUnknown(conceptRuleDeeplySatisfied)) => {

            // Doesn't matter whether the concept rule is deeply satisfied because this conforms
            // rule itself isn't satisfied yet.
            val _ = conceptRuleDeeplySatisfied
            val isaRuleSatisfied = false

            (InferMatchSuccess(isaRuleSatisfied))
          }
          case (InferEvaluateSuccess(conceptTemplata, conceptDeeplySatisfied)) => {
            val KindTemplata(sub : CitizenRef2) = subTemplata
            val KindTemplata(interface @ InterfaceRef2(_)) = conceptTemplata

            val supers = delegate.getAncestorInterfaces(state, sub)

            if (supers.contains(interface)) {
              val isaSatisfied = true
              val deeplySatisfied = subDeeplySatisfied && conceptDeeplySatisfied && isaSatisfied
              (InferMatchSuccess(deeplySatisfied))
            } else {
              return (InferMatchConflict(inferences.inferences, "Isa failed!\nSub: " + sub + "\nSuper: " + interface, List()))
            }
          }
        }
      }
    }
  }

  private[infer] def matchTemplataAgainstOrAR(
    env: Env,
    state: State,
    inferences: InferencesBox,
    instance: ITemplata,
    rule: OrAR):
  (IInferMatchResult) = {
    val OrAR(possibilities) = rule

    val results = possibilities.map(matchTemplataAgainstRulexTR(env, state, inferences, instance, _))

    // Look for one that's deeply satisfied, and return it.
    results.collect({
      case ims @ InferMatchSuccess(true) => return (ims)
    })
    // Since there were no deeply satisfied ones, look for one that matched at all.
    results.collect({
      case ims @ InferMatchSuccess(false) => return (ims)
    })
    // They must all be conflicts.
    val conflicts =
      results.map({ case imc @ InferMatchConflict(_, _, _) => imc })
    (InferMatchConflict(inferences.inferences, "No branches of the Or rule matched!", conflicts))
  }

//
//  private[infer] def matchStructAgainstOverrideSP(
//      structRef2: StructRef2,
//      overrideRule: Override1):
//  Boolean = {
//    val structDef2 = State.lookupStruct(structRef2)
//    val superInterfaces = structDef2.getAncestorInterfacesNotIncludingSelf(State)
//    val matchingInterfacesAndSubTemplars =
//      superInterfaces
//        .flatMap(interfaceRef2 => {
//          println("dont do this?")
//          val subTemplar = new InferTemplarMatcher(env, State, delegate, rules, inferences)
//          if (subTemplar.matchReferend2AgainstReferendRefSP(interfaceRef2, Some(overrideRule.tyype))) {
//            List((interfaceRef2, subTemplar))
//          } else {
//            List()
//          }
//        })
//    if (matchingInterfacesAndSubTemplars.size > 1) {
//      vfail("Can't figure for struct " + structRef2 + " which of these interfaces it implements! " + matchingInterfacesAndSubTemplars.map(_._1))
//    }
//    matchingInterfacesAndSubTemplars.headOption match {
//      case None => false
//      case Some((_, matchingSubTemplar)) => {
////        State = matchingSubTemplar.State
//        inferences = matchingSubTemplar.inferences
//        true
//      }
//    }
//  }

//  private[infer] def matchStructAgainstCitizenTemplate(instance: StructRef2, rule: CitizenTerrySP) = {
//    val StructRef2(instanceHumanName, instanceTemplateArgs) = instance
//    val CitizenTerrySP(citizenTemplateRule, templateArgRules) = rule
//
//    if (instanceTemplateArgs.size == templateArgRules.size) {
//      env.lookupTemplata(instanceHumanName) match {
//        case None => vfail("wot")
//        case Some(StructTerryTemplata(StructTerry(outerEnv, name, alreadySpecifiedExplicitTemplateArgs))) => {
//          vfail("i have no idea")
//          true
//        }
//        case Some(InterfaceTerryTemplata(InterfaceTerry(outerEnv, name, alreadySpecifiedExplicitTemplateArgs))) => {
//          vfail("wot")
//        }
//      }
//    } else {
//      // Is this possible?
//      vfail("impl?")
//    }
//  }
//
//  private[infer] def matchInterfaceAgainstCitizenTemplate(instance: InterfaceRef2, rule: CitizenTerrySP) = {
//    val InterfaceRef2(instanceHumanName, instanceTemplateArgs) = instance
//    val CitizenTerrySP(citizenTemplateRule, templateArgRules) = rule
//
//    if (instanceTemplateArgs.size == templateArgRules.size) {
//      env.lookupTemplata(instanceHumanName) match {
//        case None => vfail("wot")
//        case Some(StructTerryTemplata(StructTerry(outerEnv, name, alreadySpecifiedExplicitTemplateArgs))) => {
//          vfail("wot")
//        }
//        case Some(InterfaceTerryTemplata(InterfaceTerry(outerEnv, name, alreadySpecifiedExplicitTemplateArgs))) => {
//          vfail("i have no idea")
//          true
//        }
//      }
//    } else {
//      // Is this possible?
//      vfail("impl?")
//    }
//  }
}
