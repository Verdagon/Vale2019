package net.verdagon.radonc.templar.infer.inferer

import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.{AbstractSP, AtomSP, OverrideSP}
import net.verdagon.radonc.scout.rules._
import net.verdagon.radonc.templar.infer._
import net.verdagon.radonc.templar.templata.{Conversions, _}
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc._

import scala.collection.immutable.List
import net.verdagon.radonc.astronomer._
import net.verdagon.radonc.parser.{BorrowP, OwnP, RawP, ShareP}

trait IInfererMatcherDelegate[Env, State] {
  def lookupMemberTypes(
    state0: State,
    kind: Kind,
    // This is here so that the predictor can just give us however many things
    // we expect.
    expectedNumMembers: Int):
  Option[List[Coord]]

  def getMutability(state0: State, kind: Kind): Mutability

  def getAncestorInterfaceDistance(
    temputs0: State,
    descendantCitizenRef: CitizenRef2,
    ancestorInterfaceRef: InterfaceRef2):
  (State, Option[Int])

  def citizenIsFromTemplate(state: State, citizen: CitizenRef2, template: ITemplata): (State, Boolean)

  def getAncestorInterfaces(temputs0: State, descendantCitizenRef: CitizenRef2):
  (State, Set[InterfaceRef2])
}

class InfererMatcher[Env, State](
    templataTemplar: TemplataTemplarInner[Env, State],
    equator: InfererEquator[Env, State],
    evaluate: (Env, State, Inferences, IRulexAR) => (State, IInferEvaluateResult[ITemplata]),
    delegate: IInfererMatcherDelegate[Env, State]) {

  private[infer] def matchTemplataAgainstRuneSP(
    env: Env,
    state0: State,
    inferences0: Inferences,
    instance: ITemplata,
    rune: String,
    expectedType: ITemplataType,
    ):
  (State, IInferMatchResult) = {

    inferences0.templatasByRune.get(rune) match {
      case None => {
        val inferences1 = inferences0.addConclusion(rune, instance)
        (state0, InferMatchSuccess(inferences1, true))
      }
      case Some(alreadyInferredTemplata) => {
        val (state1, equal) =
          equator.equals(state0, instance, alreadyInferredTemplata, expectedType)
        if (equal) {
          (state1, InferMatchSuccess(inferences0, true))
        } else {
          (state1, InferMatchConflict(inferences0, s"Disagreement about templata #${rune}:\n${alreadyInferredTemplata}\n${instance}", List()))
        }
      }
    }
  }

  private[infer] def matchReference2AgainstRuneSP(
    env: Env,
    state0: State,
    inferences0: Inferences,
    instance: Coord,
    coordRune: String):
  (State, IInferMatchResult) = {

    inferences0.templatasByRune.get(coordRune) match {
      case None => {
        val inferences1 = inferences0.addConclusion(coordRune, CoordTemplata(instance))
        (state0, InferMatchSuccess(inferences1, true))
      }
      case Some(CoordTemplata(alreadyInferredCoord)) => {
        if (instance == alreadyInferredCoord) {
          (state0, InferMatchSuccess(inferences0, true))
        } else {
          (state0, InferMatchConflict(inferences0, s"Disagreement about ref #${coordRune}:\n${CoordTemplata(alreadyInferredCoord)}\n${instance}", List()))
        }
      }
    }
  }

  private[infer] def matchReferend2AgainstRuneSP(
    env: Env,
    state0: State,
    inferences0: Inferences,
    instance: Kind,
    kindRune: String):
  (State, IInferMatchResult) = {
    inferences0.templatasByRune.get(kindRune) match {
      case None => {
        val inferences1 = inferences0.addConclusion(kindRune, KindTemplata(instance))
        (state0, InferMatchSuccess(inferences1, true))
      }
      case Some(KindTemplata(alreadyInferredKind)) => {
        if (instance == alreadyInferredKind) {
          (state0, InferMatchSuccess(inferences0, true))
        } else {
          (state0, InferMatchConflict(inferences0, s"Disagreement about kind #${alreadyInferredKind}:\n${KindTemplata(alreadyInferredKind)}\n${instance}", List()))
        }
      }
    }
  }

  private[infer] def matchReference2AgainstDestructure(
    env: Env,
    state0: State,
    inferences0: Inferences,
    instance: Coord,
    parts: List[Option[AtomSP]]):
  (State, IInferMatchResult) = {

    val structMemberTypes =
      delegate.lookupMemberTypes(state0, instance.referend, expectedNumMembers = parts.size) match {
        case None => vfail("this thing cant be destructured, has no member types!")
        case Some(x) => x
      }

    val (state10, inferences10, destructuresDeeplySatisfied) =
      structMemberTypes.zip(parts).foldLeft((state0, inferences0, true))({
        case ((state1, inferences2, deeplySatisfiedSoFar), (_, None)) => {
          // The part is None; there's just an _ in this spot of the destructure.
          // Nothing to do here.
          (state1, inferences2, deeplySatisfiedSoFar)
        }
        case ((state1, inferences2, deeplySatisfiedSoFar), (structMemberType, Some(part))) => {
          val paramFilter = ParamFilter(structMemberType, None)
          matchParamFilterAgainstAtomSP(env, state1, inferences2, paramFilter, part) match {
            case (state4, imc @ InferMatchConflict(_, _, _)) => return (state4, imc)
            case (state4, InferMatchSuccess(inferences4, deeplySatisfied)) => (state4, inferences4, deeplySatisfiedSoFar && deeplySatisfied)
          }
        }
      })

    (state10, InferMatchSuccess(inferences10, destructuresDeeplySatisfied))
  }

  private[infer] def matchParamFilterAgainstAtomSP(
      env: Env,
      state0: State,
      inferences0: Inferences,
      instance: ParamFilter,
      rule: AtomSP):
  (State, IInferMatchResult) = {
    val (state1b, inferences2, coordDeeplySatisfied) =
      matchReference2AgainstRuneSP(env, state0, inferences0, instance.tyype, rule.coordRune) match {
        case (state1, imc @ InferMatchConflict(_, _, _)) => return (state1, imc)
        case (state1, InferMatchSuccess(inferences1, ds)) => (state1, inferences1, ds)
      }

    val (state2, inferences4, destructureDeeplySatisfied) =
      rule.destructure match {
        case None => (state1b, inferences2, true)
        case Some(parts) => {
          matchReference2AgainstDestructure(env, state1b, inferences0, instance.tyype, parts) match {
            case (state1, imc @ InferMatchConflict(_, _, _)) => return (state1, imc)
            case (state1, InferMatchSuccess(inferences3, ds)) => (state1, inferences3, ds)
          }
        }
      }

    val (state10, inferences10, virtualityDeeplySatisfied) =
      ((instance.virtuality, rule.virtuality) match {
        case (None, _) => (state2, inferences4, true)
        case (Some(Abstract2), Some(AbstractSP)) => (state2, inferences4, true)
        case (Some(Abstract2), _) => return (state2, InferMatchConflict(inferences4, s"ParamFilter virtuality didn't match rule:\n${instance.virtuality}\n${rule.virtuality}", List()))
        case (Some(Override2(instanceSuperInterfaceRef2)), Some(OverrideSP(kindRune))) => {
          matchReferend2AgainstRuneSP(env, state2, inferences4, instanceSuperInterfaceRef2, kindRune) match {
            case (state3, imc @ InferMatchConflict(_, _, _)) => return (state3, imc)
            case (state3, InferMatchSuccess(inferences5, ds)) => (state3, inferences5, ds)
          }
        }
        case (Some(Override2(_)), _) => return (state2, InferMatchConflict(inferences4, s"ParamFilter virtuality didn't match rule:\n${instance.virtuality}\n${rule.virtuality}", List()))
      })

    (state10, InferMatchSuccess(inferences10, coordDeeplySatisfied && destructureDeeplySatisfied && virtualityDeeplySatisfied))
  }

  private[infer] def matchCitizenAgainstCallTT(
    env: Env,
    state0: State,
    inferences0: Inferences,
    call: CallAT,
    actualCitizen: CitizenRef2):
  (State, IInferMatchResult) = {
    evaluate(env, state0, inferences0, TemplexAR(call.template)) match {
      case (state1, iec @ InferEvaluateConflict(_, _, _)) => return (state1, InferMatchConflict(inferences0, "Couldn't evaluate template!", List(iec)))
      case (state1, InferEvaluateUnknown(inferences1, _)) => {
        vcurious() // Can this ever happen? If it does, is the below conflict appropriate?
        (state1, InferMatchConflict(inferences1, "Couldn't figure out template!", List()))
      }
      case (state1, InferEvaluateSuccess(inferences1, callTemplateTemplata, templateDeeplySatisfied)) => {
        // debt: TEST THIS!

        val state3 =
          delegate.citizenIsFromTemplate(state1, actualCitizen, callTemplateTemplata) match {
            case (state2, false) => return (state2, InferMatchConflict(inferences1, "Given citizen didn't come from expected template!\nCitizen: " + actualCitizen + "\nTemplate: " + callTemplateTemplata, List()))
            case (state2, true) => state2
          }

        val expectedArgs = call.args
        if (actualCitizen.fullName.steps.size > 1) {
          vimpl()
        }
        val actualArgs = actualCitizen.fullName.steps.last.templateArgs.get

        // Check to see that the actual template args match the expected template args
        val (state15, inferences5, argsDeeplySatisfied) =
          expectedArgs.zip(actualArgs).foldLeft((state3, inferences1, true))({
            case ((state13, inferences3, deeplySatisfiedSoFar), (expectedArg, actualArg)) => {
              matchTemplataAgainstTemplexAR(env, state13, inferences3, actualArg, expectedArg) match {
                case (state14, imc @ InferMatchConflict(_, _, _)) => return (state14, imc)
                case (state14, InferMatchSuccess(inferences4, deeplySatisfied)) => (state14, inferences4, deeplySatisfiedSoFar && deeplySatisfied)
              }
            }
          })
        // If the function is the same, and the args are the same... it's the same.
        (state15, InferMatchSuccess(inferences5, templateDeeplySatisfied && argsDeeplySatisfied))
      }
    }
  }

  private[infer] def matchArrayAgainstCallTT(
    env: Env,
    state0: State,
    inferences0: Inferences,
    expectedTemplate: ITemplexA,
    expectedArgs: List[ITemplexA],
    actualArgs: List[ITemplata]):
  (State, IInferMatchResult) = {
    // Check to see that the actual template matches the expected template
    val (state2, inferences2, templateDeeplySatisfied) =
      matchTemplataAgainstTemplexAR(env, state0, inferences0, ArrayTemplateTemplata(), expectedTemplate) match {
        case (state1b, imc @ InferMatchConflict(_, _, _)) => return (state1b, imc)
        case (state1b, InferMatchSuccess(inferences1, ds)) => (state1b, inferences1, ds)
      }
    // Check to see that the actual template args match the expected template args
    val (state5, inferences5, argsDeeplySatisfied) =
      expectedArgs.zip(actualArgs).foldLeft((state2, inferences2, true))({
        case ((state3, inferences3, deeplySatisfiedSoFar), (expectedArg, actualArg)) => {
          matchTemplataAgainstTemplexAR(env, state3, inferences3, actualArg, expectedArg) match {
            case (state4, imc @ InferMatchConflict(_, _, _)) => return (state4, imc)
            case (state4, InferMatchSuccess(inferences4, deeplySatisfied)) => (state4, inferences4, deeplySatisfiedSoFar && deeplySatisfied)
          }
        }
      })
    // If the function is the same, and the args are the same... it's the same.
    (state5, InferMatchSuccess(inferences5, templateDeeplySatisfied && argsDeeplySatisfied))
  }

  private[infer] def matchTemplataAgainstTemplexAR(
      env: Env,
      state0: State,
      inferences0: Inferences,
      instance: ITemplata,
      rule: ITemplexA):
  (State, IInferMatchResult) = {
    (rule, instance) match {
      case (AnonymousRuneAT(expectedType), _) => {
        vassert(instance.tyype == expectedType)
        (state0, InferMatchSuccess(inferences0, true))
      }
      case (IntAT(expectedValue), IntegerTemplata(actualValue))
          if actualValue == expectedValue => {
        (state0, InferMatchSuccess(inferences0, true))
      }
      case (BoolAT(expectedValue), BooleanTemplata(actualValue))
          if actualValue == expectedValue => {
        (state0, InferMatchSuccess(inferences0, true))
      }
      case (OwnershipAT(expectedOwnership), OwnershipTemplata(actualOwnership)) => {
        if (actualOwnership == Conversions.evaluateOwnership(expectedOwnership)) {
          return (state0, InferMatchSuccess(inferences0, true))
        } else if (actualOwnership == Share) {
          // Share is compatible with anything except raw
          if (expectedOwnership != RawP) {
            return (state0, InferMatchSuccess(inferences0, true))
          }
        } else if (actualOwnership == Raw) {
          // Raw is compatible with anything except share
          if (expectedOwnership != ShareP) {
            return (state0, InferMatchSuccess(inferences0, true))
          }
        }
        return (state0, InferMatchConflict(inferences0, s"Supplied ${actualOwnership} doesn't match expected ${expectedOwnership}", List()))
      }
      case (MutabilityAT(expectedMutability), MutabilityTemplata(actualMutability)) => {
        if (actualMutability == Conversions.evaluateMutability(expectedMutability)) {
          (state0, InferMatchSuccess(inferences0, true))
        } else {
          return (state0, InferMatchConflict(inferences0, s"Supplied ${actualMutability} doesn't match expected ${expectedMutability}", List()))
        }
      }
      case (PermissionAT(expectedPermission), PermissionTemplata(actualPermission)) => {
        if (actualPermission == Conversions.evaluatePermission(expectedPermission)) {
          (state0, InferMatchSuccess(inferences0, true))
        } else {
          return (state0, InferMatchConflict(inferences0, s"Supplied ${actualPermission} doesn't match expected ${expectedPermission}", List()))
        }
      }
      case (LocationAT(expectedLocation), LocationTemplata(actualLocation)) => {
        if (actualLocation == Conversions.evaluateLocation(expectedLocation)) {
          (state0, InferMatchSuccess(inferences0, true))
        } else {
          return (state0, InferMatchConflict(inferences0, s"Supplied ${actualLocation} doesn't match expected ${expectedLocation}", List()))
        }
      }
      case (VariabilityAT(expectedVariability), VariabilityTemplata(actualVariability)) => {
        if (actualVariability == Conversions.evaluateVariability(expectedVariability)) {
          (state0, InferMatchSuccess(inferences0, true))
        } else {
          return (state0, InferMatchConflict(inferences0, s"Supplied ${actualVariability} doesn't match expected ${expectedVariability}", List()))
        }
      }
      case (NameAT(name, expectedType), actualTemplata) => {
        val (state1, expectedTemplata) = templataTemplar.lookupTemplata(env, state0, name, expectedType)
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
          return (state1, InferMatchConflict(inferences0, s"Supplied templata doesn't match '${name}':\n'${name}' in environment:${expectedTemplata}\nActual:${actualTemplata}", List()))
        }
        (state1, InferMatchSuccess(inferences0, true))
      }
      case (RuneAT(rune, expectedType), actualTemplata) => {
        if (actualTemplata.tyype != expectedType) {
          return (state0, InferMatchConflict(inferences0, s"Doesn't match type! Expected ${expectedType} but received ${actualTemplata}", List()))
        }
        matchTemplataAgainstRuneSP(env, state0, inferences0, actualTemplata, rune, expectedType) match {
          case (state1, imc @ InferMatchConflict(_, _, _)) => return (state1, imc)
          case (state1, imc @ InferMatchSuccess(_, _)) => (state1, imc)
        }
      }
      case (ct @ CallAT(_, _, _), CoordTemplata(Coord(_, cit @ StructRef2(_)))) => {
        vassert(instance.tyype == ct.resultType)
        matchCitizenAgainstCallTT(
          env, state0, inferences0, ct, cit)
      }
      case (ct @ CallAT(_, _, _), CoordTemplata(Coord(_, cit @ InterfaceRef2(_)))) => {
        vassert(instance.tyype == ct.resultType)
        matchCitizenAgainstCallTT(
          env, state0, inferences0, ct, cit)
      }
      case (ct @ CallAT(_, _, _), KindTemplata(cit @ StructRef2(_))) => {
        vassert(instance.tyype == ct.resultType)
        matchCitizenAgainstCallTT(
          env, state0, inferences0, ct, cit)
      }
      case (ct @ CallAT(_, _, _), KindTemplata(cit @ InterfaceRef2(_))) => {
        vassert(instance.tyype == ct.resultType)
        matchCitizenAgainstCallTT(
          env, state0, inferences0, ct, cit)
      }
      case (CallAT(expectedTemplate, expectedArgs, resultType), KindTemplata(UnknownSizeArrayT2(RawArrayT2(elementArg,mutability)))) => {
        vassert(instance.tyype == resultType)
        matchArrayAgainstCallTT(
          env, state0, inferences0, expectedTemplate, expectedArgs, List(MutabilityTemplata(mutability), CoordTemplata(elementArg)))
      }
      case (CallAT(expectedTemplate, expectedArgs, resultType), KindTemplata(ArraySequenceT2(size, RawArrayT2(elementArg,mutability)))) => {
        return (state0, InferMatchConflict(inferences0, "Can't match array sequence against anything, no such rule exists", List()))
      }
      case (CallAT(expectedTemplate, expectedArgs, resultType), CoordTemplata(Coord(_, ArraySequenceT2(_, _)))) => {
        return (state0, InferMatchConflict(inferences0, "Can't match array sequence against anything, no such rule exists", List()))
      }
      case (CallAT(expectedTemplate, expectedArgs, resultType), CoordTemplata(Coord(instanceOwnership, UnknownSizeArrayT2(RawArrayT2(elementArg,mutability))))) => {
        vassert(instance.tyype == resultType)
        matchArrayAgainstCallTT(
          env, state0, inferences0, expectedTemplate, expectedArgs, List(MutabilityTemplata(mutability), CoordTemplata(elementArg)))
      }
      case (PrototypeAT(_, _, _), _) => {
        vfail("what even is this")
      }
      case (PackAT(expectedMembers, _), KindTemplata(PackT2(actualMembers, _))) => {
        val (state5, inferences5, membersDeeplySatisfied) =
          expectedMembers.zip(actualMembers).foldLeft((state0, inferences0, true))({
            case ((state1, inferences1, deeplySatisfiedSoFar), (expectedMember, actualMember)) => {
              matchTemplataAgainstTemplexAR(env, state1, inferences1, CoordTemplata(actualMember), expectedMember) match {
                case (state2, imc @ InferMatchConflict(_, _, _)) => return (state2, imc)
                case (state2, InferMatchSuccess(inferences2, deeplySatisfied)) => (state2, inferences2, deeplySatisfiedSoFar && deeplySatisfied)
              }
            }
          })
        (state5, InferMatchSuccess(inferences5, membersDeeplySatisfied))
      }
      case (RepeaterSequenceAT(mutabilityTemplex, sizeTemplex, elementTemplex, resultType), CoordTemplata(Coord(ownership, ArraySequenceT2(size, RawArrayT2(elementCoord, mutability))))) => {
        vassert(resultType == CoordTemplataType)
        vcurious(ownership == Share || ownership == Own)

        val (state5, inferences5, mutabilityDeeplySatisfied) =
          matchTemplataAgainstTemplexAR(env, state0, inferences0, MutabilityTemplata(mutability), mutabilityTemplex) match {
            case (state2, imc @ InferMatchConflict(_, _, _)) => return (state2, imc)
            case (state2, InferMatchSuccess(inferences2, deeplySatisfied)) => (state2, inferences2, deeplySatisfied)
          }

        val (state10, inferences10, sizeDeeplySatisfied) =
          matchTemplataAgainstTemplexAR(env, state5, inferences5, IntegerTemplata(size), sizeTemplex) match {
            case (state7, imc @ InferMatchConflict(_, _, _)) => return (state7, imc)
            case (state7, InferMatchSuccess(inferences7, deeplySatisfied)) => (state7, inferences7, deeplySatisfied)
          }

        val (state15, inferences15, elementDeeplySatisfied) =
          matchTemplataAgainstTemplexAR(env, state10, inferences10, CoordTemplata(elementCoord), elementTemplex) match {
            case (state12, imc @ InferMatchConflict(_, _, _)) => return (state12, imc)
            case (state12, InferMatchSuccess(inferences12, deeplySatisfied)) => (state12, inferences12, deeplySatisfied)
          }

        val deeplySatisfied = mutabilityDeeplySatisfied && sizeDeeplySatisfied && elementDeeplySatisfied
        (state15, InferMatchSuccess(inferences15, deeplySatisfied))
      }
      case (RepeaterSequenceAT(mutabilityTemplex, sizeTemplex, elementTemplex, resultType), KindTemplata(ArraySequenceT2(size, RawArrayT2(elementCoord, mutability)))) => {
        vassert(resultType == KindTemplataType)

        val (state5, inferences5, mutabilityDeeplySatisfied) =
          matchTemplataAgainstTemplexAR(env, state0, inferences0, MutabilityTemplata(mutability), mutabilityTemplex) match {
            case (state2, imc @ InferMatchConflict(_, _, _)) => return (state2, imc)
            case (state2, InferMatchSuccess(inferences2, deeplySatisfied)) => (state2, inferences2, deeplySatisfied)
          }

        val (state10, inferences10, sizeDeeplySatisfied) =
          matchTemplataAgainstTemplexAR(env, state5, inferences5, IntegerTemplata(size), sizeTemplex) match {
            case (state7, imc @ InferMatchConflict(_, _, _)) => return (state7, imc)
            case (state7, InferMatchSuccess(inferences7, deeplySatisfied)) => (state7, inferences7, deeplySatisfied)
          }

        val (state15, inferences15, elementDeeplySatisfied) =
          matchTemplataAgainstTemplexAR(env, state10, inferences10, CoordTemplata(elementCoord), elementTemplex) match {
            case (state12, imc @ InferMatchConflict(_, _, _)) => return (state12, imc)
            case (state12, InferMatchSuccess(inferences12, deeplySatisfied)) => (state12, inferences12, deeplySatisfied)
          }

        val deeplySatisfied = mutabilityDeeplySatisfied && sizeDeeplySatisfied && elementDeeplySatisfied
        (state15, InferMatchSuccess(inferences15, deeplySatisfied))
      }
      case (RepeaterSequenceAT(_, _, _, _), KindTemplata(otherKind)) => {
        (state0, InferMatchConflict(inferences0, "Expected repeater sequence, was: " + otherKind, List()))
      }
      case (RepeaterSequenceAT(_, _, _, _), CoordTemplata(otherCoord)) => {
        (state0, InferMatchConflict(inferences0, "Expected repeater sequence, was: " + otherCoord, List()))
      }
      case (OwnershipAT(ownershipP), OwnershipTemplata(ownershipT)) => {
        if (ownershipT == Share) {
          // Doesn't matter what the ownership rule was, ownership doesnt apply to Share.
          (state0, InferMatchSuccess(inferences0, true))
        } else if (ownershipT == Conversions.evaluateOwnership(ownershipP)) {
          (state0, InferMatchSuccess(inferences0, true))
        } else {
          (state0, InferMatchConflict(inferences0, s"Ownerships don't match: ${ownershipP} and ${ownershipT}", List()))
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
            matchTemplataAgainstTemplexAR(env, state0, inferences0, CoordTemplata(Coord(Own, instanceKind)), innerCoordTemplex)
          } else {
            matchTemplataAgainstTemplexAR(env, state0, inferences0, CoordTemplata(Coord(instanceOwnership, instanceKind)), innerCoordTemplex)
          }
        } else {
          (state0, InferMatchConflict(inferences0, s"Couldn't match incoming ${instanceOwnership} against expected ${expectedOwnership}", List()))
        }
      }
      case _ => vfail("Can't match rule " + rule + " against instance " + instance)
    }
  }

  private[infer] def matchTemplataAgainstRulexTR(
    env: Env,
    state0: State,
    inferences0: Inferences,
    instance: ITemplata,
    irule: IRulexAR):
  (State, IInferMatchResult) = {
    irule match {
      case rule @ EqualsAR(_, _) => {
        matchTemplataAgainstEqualsAR(env, state0, inferences0, instance, rule)
      }
      case rule @ IsaAR(_, _) => {
        matchTemplataAgainstIsaAR(env, state0, inferences0, instance, rule)
      }
      case rule @ OrAR(_) => {
        matchTemplataAgainstOrAR(env, state0, inferences0, instance, rule)
      }
      case rule @ ComponentsAR(_, _) => {
        matchTemplataAgainstComponentsAR(env, state0, inferences0, instance, rule)
      }
      case TemplexAR(itemplexTT) => {
        matchTemplataAgainstTemplexAR(env, state0, inferences0, instance, itemplexTT)
      }
      case rule @ CallAR(_, _, _) => {
        matchTemplataAgainstCallAR(env, state0, inferences0, instance, rule)
      }
    }
  }

  // debt: rename from instance
  private[infer] def matchTemplataAgainstCallAR(
    env: Env,
    state0: State,
    inferences0: Inferences,
    instance: ITemplata,
    rule: CallAR):
  (State, IInferMatchResult) = {
    vassert(instance.tyype == rule.resultType)

    // We don't match into the argRules here, see MDMIA.
    // But we can't just *not* match them, and evaluating could return unknown, in which case we
    // don't know what to do.
    // For now, we'll only allow calls like toRef that are 1:1, and so can be matched.

    val CallAR(name, args, resultType) = rule

    if (instance.tyype != resultType) {
      return (state0, InferMatchConflict(inferences0, "Call result expected type " + resultType + ", but was " + instance, List()))
    }

    name match {
      case "toRef" => {
        val List(kindRule) = args
        instance match {
          case CoordTemplata(Coord(instanceOwnership, instanceKind)) => {
            val defaultOwnershipForKind =
              if (delegate.getMutability(state0, instanceKind) == Mutable) Own else Share
            if (instanceOwnership != defaultOwnershipForKind) {
              return (state0, InferMatchConflict(inferences0, "Coord matching into toRef doesn't have default ownership: " + instanceOwnership, List()))
            }
            matchTemplataAgainstRulexTR(env, state0, inferences0, KindTemplata(instanceKind), kindRule)
          }
          case _ => return (state0, InferMatchConflict(inferences0, "Bad arguments to toRef: " + args, List()))
        }
      }
      case "passThroughIfConcrete" => {
        val List(kindRule) = args
        instance match {
          case KindTemplata(StructRef2(_) | PackT2(_, _) | TupleT2(_, _) | ArraySequenceT2(_, _) | UnknownSizeArrayT2(_)) => {
            matchTemplataAgainstRulexTR(env, state0, inferences0, instance, kindRule)
          }
          case _ => return (state0, InferMatchConflict(inferences0, "Bad arguments to passThroughIfConcrete: " + args, List()))
        }
      }
      case "passThroughIfInterface" => {
        val List(kindRule) = args
        instance match {
          case KindTemplata(InterfaceRef2(_)) => {
            matchTemplataAgainstRulexTR(env, state0, inferences0, instance, kindRule)
          }
          case _ => return (state0, InferMatchConflict(inferences0, "Bad arguments to passThroughIfInterface: " + args, List()))
        }
      }
      case "passThroughIfStruct" => {
        val List(kindRule) = args
        instance match {
          case KindTemplata(StructRef2(_)) => {
            matchTemplataAgainstRulexTR(env, state0, inferences0, instance, kindRule)
          }
          case _ => return (state0, InferMatchConflict(inferences0, "Bad arguments to passThroughIfStruct: " + args, List()))
        }
      }
    }
  }

  private[infer] def matchTemplataAgainstComponentsAR(
    env: Env,
    state0: State,
    inferences0: Inferences,
    instance: ITemplata,
    rule: ComponentsAR):
  (State, IInferMatchResult) = {
    val ComponentsAR(tyype, components) = rule

    if (!equator.templataMatchesType(instance, tyype)) {
      return (state0, InferMatchConflict(inferences0, s"Supplied templata isn't the right type! Type: ${rule.tyype} but gave: ${instance}", List()))
    }

    instance match {
      case KindTemplata(actualReferend) => {
        components match {
          case List(mutabilityRule) => {
            val actualMutability = delegate.getMutability(state0, actualReferend)
            matchTemplataAgainstRulexTR(
              env, state0, inferences0, MutabilityTemplata(actualMutability), mutabilityRule)
          }
          case _ => vfail("Wrong number of components for kind")
        }
      }
      case CoordTemplata(actualReference) => {
        components match {
          case List(ownershipRule, kindRule) => {
            val actualOwnership = OwnershipTemplata(actualReference.ownership)
            val (state2, inferences2, ownershipDeeplySatisfied) =
              matchTemplataAgainstRulexTR(env, state0, inferences0, actualOwnership, ownershipRule) match {
                case (state1b, imc @ InferMatchConflict(_, _, _)) => return (state1b, imc)
                case (state1b, InferMatchSuccess(inferences1b, ods)) => (state1b, inferences1b, ods)
              }
            val actualKind = KindTemplata(actualReference.referend)
            val (state8, inferences8, kindDeeplySatisfied) =
              matchTemplataAgainstRulexTR(env, state2, inferences2, actualKind, kindRule) match {
                case (state7, imc @ InferMatchConflict(_, _, _)) => return (state7, imc)
                case (state7, InferMatchSuccess(inferences7, kds)) => (state7, inferences7, kds)
              }
            (state8, InferMatchSuccess(inferences8, ownershipDeeplySatisfied && kindDeeplySatisfied))
          }
          case _ => vfail("Wrong number of components for kind")
        }
      }
    }
  }

  private[infer] def matchTemplataAgainstEqualsAR(
    env: Env,
    state0: State,
    inferences0: Inferences,
    instance: ITemplata,
    rule: EqualsAR):
  (State, IInferMatchResult) = {
    val EqualsAR(left, right) = rule

    matchTemplataAgainstRulexTR(env, state0, inferences0, instance, left) match {
      case (state1, imc @ InferMatchConflict(_, _, _)) => (state1, imc)
      case (state1, InferMatchSuccess(inferences1, leftDeeplySatisfied)) => {
        matchTemplataAgainstRulexTR(env, state1, inferences1, instance, right) match {
          case (state2, imc @ InferMatchConflict(_, _, _)) => (state2, imc)
          case (state2, InferMatchSuccess(inferences2, rightDeeplySatisfied)) => {
            (state2, InferMatchSuccess(inferences2, leftDeeplySatisfied && rightDeeplySatisfied))
          }
        }
      }
    }
  }

  private[infer] def matchTemplataAgainstIsaAR(
    env: Env,
    state0: State,
    inferences0: Inferences,
    subTemplata: ITemplata,
    rule: IsaAR):
  (State, IInferMatchResult) = {
    val IsaAR(left, right) = rule

    matchTemplataAgainstRulexTR(env, state0, inferences0, subTemplata, left) match {
      case (state1, imc @ InferMatchConflict(_, _, _)) => (state1, imc)
      case (state1, InferMatchSuccess(inferences1, subDeeplySatisfied)) => {
        evaluate(env, state1, inferences1, right) match {
          case (state2, iec @ InferEvaluateConflict(_, _, _)) => return (state2, InferMatchConflict(inferences1, "Couldn't evaluate concept!", List(iec)))
          case (state2, InferEvaluateUnknown(inferences2, conceptRuleDeeplySatisfied)) => {

            // Doesn't matter whether the concept rule is deeply satisfied because this conforms
            // rule itself isn't satisfied yet.
            val (_) = conceptRuleDeeplySatisfied
            val isaRuleSatisfied = false

            (state2, InferMatchSuccess(inferences2, isaRuleSatisfied))
          }
          case (state2, InferEvaluateSuccess(inferences2, conceptTemplata, conceptDeeplySatisfied)) => {
            val KindTemplata(sub : CitizenRef2) = subTemplata
            val KindTemplata(interface @ InterfaceRef2(_)) = conceptTemplata

            val (state30, supers) = delegate.getAncestorInterfaces(state2, sub)

            if (supers.contains(interface)) {
              val isaSatisfied = true
              val deeplySatisfied = subDeeplySatisfied && conceptDeeplySatisfied && isaSatisfied
              (state30, InferMatchSuccess(inferences2, deeplySatisfied))
            } else {
              return (state30, InferMatchConflict(inferences2, "Isa failed!\nSub: " + sub + "\nSuper: " + interface, List()))
            }
          }
        }
      }
    }
  }

  private[infer] def matchTemplataAgainstOrAR(
    env: Env,
    state0: State,
    inferences0: Inferences,
    instance: ITemplata,
    rule: OrAR):
  (State, IInferMatchResult) = {
    val OrAR(possibilities) = rule

    val (state10, results) =
      possibilities.foldLeft((state0, List[IInferMatchResult]()))({
        case ((state1, previousResults), possibility) => {
          val (state2, result) =
            matchTemplataAgainstRulexTR(env, state1, inferences0, instance, possibility)
          (state2, previousResults :+ result)
        }
      })

    // Look for one that's deeply satisfied, and return it.
    results.collect({
      case ims @ InferMatchSuccess(_, true) => return (state10, ims)
    })
    // Since there were no deeply satisfied ones, look for one that matched at all.
    results.collect({
      case ims @ InferMatchSuccess(_, false) => return (state10, ims)
    })
    // They must all be conflicts.
    val conflicts =
      results.map({ case imc @ InferMatchConflict(_, _, _) => imc })
    (state10, InferMatchConflict(inferences0, "No branches of the Or rule matched!", conflicts))
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
