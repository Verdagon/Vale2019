package net.verdagon.vale.templar.infer

import net.verdagon.vale.astronomer._
import net.verdagon.vale.scout.patterns.AtomSP
import net.verdagon.vale.scout.{FunctionS, InterfaceS, StructS}
import net.verdagon.vale.templar.Temputs
import net.verdagon.vale.templar.env.{IEnvironment, TemplataLookupContext}
import net.verdagon.vale.templar.infer.inferer._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{vassertSome, vfail, vimpl}

import scala.collection.immutable.List

trait IInfererDelegate[Env, State] {
  def evaluateType(
    env: Env,
    state: State,
    type1: ITemplexA):
  (ITemplata)

  def lookupMemberTypes(
    state: State,
    kind: Kind,
    // This is here so that the predictor can just give us however many things
    // we expect.
    expectedNumMembers: Int
  ): Option[List[Coord]]

  def getMutability(state: State, kind: Kind): Mutability

  def lookupTemplata(env: Env, name: AbsoluteNameA[INameA]): ITemplata
  def lookupTemplata(env: Env, name: ImpreciseNameA[IImpreciseNameStepA]): ITemplata

  def evaluateStructTemplata(
    state: State,
    templata: StructTemplata,
    templateArgs: List[ITemplata]):
  (Kind)

  def evaluateInterfaceTemplata(
    state: State,
    templata: InterfaceTemplata,
    templateArgs: List[ITemplata]):
  (Kind)

  def getPackKind(env: Env, state: State, members: List[Coord]): (PackT2, Mutability)

  def getArraySequenceKind(env: Env, state: State, mutability: Mutability, size: Int, element: Coord): (ArraySequenceT2)

  def getAncestorInterfaceDistance(temputs: State, descendantCitizenRef: CitizenRef2, ancestorInterfaceRef: InterfaceRef2): (Option[Int])

  def getAncestorInterfaces(temputs: State, descendantCitizenRef: CitizenRef2):
  (Set[InterfaceRef2])

  def getInterfaceTemplataType(it: InterfaceTemplata): ITemplataType
  def getStructTemplataType(st: StructTemplata): ITemplataType

  def getMemberCoords(state: State, structRef: StructRef2): List[Coord]

  def citizenIsFromTemplate(state: State, citizen: CitizenRef2, template: ITemplata): (Boolean)

  def structIsClosure(state: State, structRef: StructRef2): Boolean

  def getSimpleInterfaceMethod(state: State, interfaceRef: InterfaceRef2): Prototype2
}

// This is the public API for the outside world to use the Infer code.
object Inferer {
  def solve[Env, State](
    delegate: IInfererDelegate[Env, State],
    env: Env,
    state: State,
    rules: List[IRulexAR],
    typeByRune: Map[AbsoluteNameA[IRuneA], ITemplataType],
    directInputs: Map[AbsoluteNameA[IRuneA], ITemplata],
    paramAtoms: List[AtomAP],
    maybeParamInputs: Option[List[ParamFilter]],
    checkAllRunesPresent: Boolean):
  (IInferSolveResult) = {
    val templataTemplar =
      new TemplataTemplarInner[Env, State](makeTemplataTemplarDelegate(delegate))
    val equalsLayer = new InfererEquator[Env, State](templataTemplar)
    val templar =
      new InfererEvaluator[Env, State](
        templataTemplar,
        equalsLayer,
        makeEvaluatorDelegate(delegate))
    templar.solve(
      env,
      state,
      rules,
      typeByRune,
      directInputs,
      paramAtoms,
      maybeParamInputs,
      checkAllRunesPresent)
  }

  def makeTemplataTemplarDelegate[Env, State](
    delegate: IInfererDelegate[Env, State]):
  (ITemplataTemplarInnerDelegate[Env, State]) = {
    new ITemplataTemplarInnerDelegate[Env, State] {
      override def getAncestorInterfaceDistance(temputs: State, descendantCitizenRef: CitizenRef2, ancestorInterfaceRef: InterfaceRef2): (Option[Int]) = {
        delegate.getAncestorInterfaceDistance(temputs, descendantCitizenRef, ancestorInterfaceRef)
      }
      override def getMutability(state: State, kind: Kind): Mutability = {
        delegate.getMutability(state, kind)
      }

      override def getPackKind(env: Env, state: State, members: List[Coord]): (PackT2, Mutability) = {
        delegate.getPackKind(env, state, members)
      }

      override def lookupTemplata(env: Env, name: AbsoluteNameA[INameA]): ITemplata = {
        delegate.lookupTemplata(env, name)
      }

      override def lookupTemplata(env: Env, name: ImpreciseNameA[IImpreciseNameStepA]): ITemplata = {
        delegate.lookupTemplata(env, name)
      }

      override def evaluateInterfaceTemplata(state: State, templata: InterfaceTemplata, templateArgs: List[ITemplata]): (Kind) = {
        delegate.evaluateInterfaceTemplata(state, templata, templateArgs)
      }

      override def evaluateStructTemplata(state: State, templata: StructTemplata, templateArgs: List[ITemplata]): (Kind) = {
        delegate.evaluateStructTemplata(state, templata, templateArgs)
      }

      override def getArraySequenceKind(env: Env, state: State, mutability: Mutability, size: Int, element: Coord): (ArraySequenceT2) = {
        delegate.getArraySequenceKind(env, state, mutability, size, element)
      }

      override def getInterfaceTemplataType(it: InterfaceTemplata): ITemplataType = {
        delegate.getInterfaceTemplataType(it)
      }

      override def getStructTemplataType(st: StructTemplata): ITemplataType = {
        delegate.getStructTemplataType(st)
      }
    }
  }

  def coerce(templata: ITemplata, tyype: ITemplataType): ITemplata = {
    (templata, tyype) match {
      case _ => vimpl()
    }
  }

  private def makeEvaluatorDelegate[Env, State](delegate: IInfererDelegate[Env, State]):
  IInfererEvaluatorDelegate[Env, State] = {
    new IInfererEvaluatorDelegate[Env, State] {
      override def getAncestorInterfaces(temputs: State, descendantCitizenRef: CitizenRef2): (Set[InterfaceRef2]) = {
        delegate.getAncestorInterfaces(temputs, descendantCitizenRef)
      }

      override def lookupMemberTypes(state: State, kind: Kind, expectedNumMembers: Int):
      Option[List[Coord]] = {
        delegate.lookupMemberTypes(state, kind, expectedNumMembers)
      }

      override def getMutability(state: State, kind: Kind): Mutability = {
        delegate.getMutability(state: State, kind: Kind)
      }

      override def getAncestorInterfaceDistance(temputs: State, descendantCitizenRef: CitizenRef2, ancestorInterfaceRef: InterfaceRef2): (Option[Int]) = {
        delegate.getAncestorInterfaceDistance(temputs, descendantCitizenRef, ancestorInterfaceRef)
      }

      override def getMemberCoords(state: State, structRef: StructRef2): List[Coord] = {
        delegate.getMemberCoords(state, structRef)
      }

      override def citizenIsFromTemplate(state: State, citizen: CitizenRef2, template: ITemplata): (Boolean) = {
        delegate.citizenIsFromTemplate(state, citizen, template)
      }

      override def structIsClosure(state: State, structRef: StructRef2): Boolean = {
        delegate.structIsClosure(state, structRef)
      }

      override def getSimpleInterfaceMethod(state: State, interfaceRef: InterfaceRef2): Prototype2 = {
        delegate.getSimpleInterfaceMethod(state, interfaceRef)
      }
    }
  }
}
