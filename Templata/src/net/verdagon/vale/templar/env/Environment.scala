package net.verdagon.vale.templar.env

import net.verdagon.vale.astronomer._
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar.types.{FullName2, NamePart2}
import net.verdagon.vale.{vassert, vfail, vimpl, vwat}

import scala.collection.immutable.{List, Map}

trait IEnvironment {
  override def toString: String = {
    "#Environment"
  }
  def globalEnv: NamespaceEnvironment
  def getAllTemplatasWithAbsoluteName(name: AbsoluteNameA[INameA], lookupFilter: Set[ILookupContext]): List[ITemplata]
  def getNearestTemplataWithAbsoluteName(name: AbsoluteNameA[INameA], lookupFilter: Set[ILookupContext]): Option[ITemplata]
  def getAllTemplatasWithName(name: ImpreciseNameA[IImpreciseNameStepA], lookupFilter: Set[ILookupContext]): List[ITemplata]
  def getNearestTemplataWithName(name: ImpreciseNameA[IImpreciseNameStepA], lookupFilter: Set[ILookupContext]): Option[ITemplata]
  def fullName: FullName2
}

trait IEnvironmentBox {
  def snapshot: IEnvironment
  override def toString: String = {
    "#Environment"
  }
  def globalEnv: NamespaceEnvironment
  def getAllTemplatasWithAbsoluteName(name: AbsoluteNameA[INameA], lookupFilter: Set[ILookupContext]): List[ITemplata]
  def getNearestTemplataWithAbsoluteName(name: AbsoluteNameA[INameA], lookupFilter: Set[ILookupContext]): Option[ITemplata]
  def fullName: FullName2
}

sealed trait ILookupContext
case object TemplataLookupContext extends ILookupContext
case object ExpressionLookupContext extends ILookupContext

case class NamespaceEnvironment(
  maybeParentEnv: Option[IEnvironment],
  fullName: FullName2,
  entries: Map[AbsoluteNameA[INameA], List[IEnvEntry]]
) extends IEnvironment {
  maybeParentEnv match {
    case None =>
    case Some(parentEnv) => vassert(fullName.steps.startsWith(parentEnv.fullName.steps))
  }

  override def globalEnv: NamespaceEnvironment = {
    maybeParentEnv match {
      case None => this
      case Some(parentEnv) => parentEnv.globalEnv
    }
  }

  override def getAllTemplatasWithAbsoluteName(name: AbsoluteNameA[INameA], lookupFilter: Set[ILookupContext]): List[ITemplata] = {
    entries.getOrElse(name, List())
      .filter(EnvironmentUtils.entryMatchesFilter(_, lookupFilter))
      .map(EnvironmentUtils.entryToTemplata(this, _))++
      maybeParentEnv.toList.flatMap(_.getAllTemplatasWithAbsoluteName(name, lookupFilter))
  }

  override def getNearestTemplataWithAbsoluteName(name: AbsoluteNameA[INameA], lookupFilter: Set[ILookupContext]): Option[ITemplata] = {
    entries
      .get(name).toList.flatten
      .filter(EnvironmentUtils.entryMatchesFilter(_, lookupFilter)) match {
      case List(entry) => Some(EnvironmentUtils.entryToTemplata(this, entry))
      case List() => {
        maybeParentEnv match {
          case None => None
          case Some(parentEnv) => parentEnv.getNearestTemplataWithAbsoluteName(name, lookupFilter)
        }
      }
      case multiple => vfail("Too many things named " + name + ":\n" + multiple.mkString("\n"));
    }
  }

  override def getAllTemplatasWithName(name: ImpreciseNameA[IImpreciseNameStepA], lookupFilter: Set[ILookupContext]): List[ITemplata] = {
    vimpl()
  }

  override def getNearestTemplataWithName(name: ImpreciseNameA[IImpreciseNameStepA], lookupFilter: Set[ILookupContext]): Option[ITemplata] = {
    vimpl()
  }

  def addFunction(function: FunctionA): NamespaceEnvironment = {
    NamespaceEnvironment(
      maybeParentEnv,
      fullName,
      EnvironmentUtils.addFunction(entries, function))
  }

  def addEntry(name: AbsoluteNameA[INameA], entry: IEnvEntry): NamespaceEnvironment = {
    NamespaceEnvironment(
      maybeParentEnv,
      fullName,
      EnvironmentUtils.addEntry(entries, name, entry))
  }

  def addEntries(newEntries: Map[AbsoluteNameA[INameA], List[IEnvEntry]]): NamespaceEnvironment = {
    NamespaceEnvironment(
      maybeParentEnv,
      fullName,
      EnvironmentUtils.addEntries(entries, newEntries))
  }
}

object EnvironmentUtils {
  def entryToTemplata(env: IEnvironment, entry: IEnvEntry): ITemplata = {
    entry match {
      case FunctionEnvEntry(func) => {
        println("Fill in the containers!")
        FunctionTemplata(env, List(), func)
      }
      case StructEnvEntry(struct) => {
        StructTemplata(NamespaceEnvironment(Some(env), env.fullName, Map()), struct)
      }
      case InterfaceEnvEntry(interface) => {
        InterfaceTemplata(NamespaceEnvironment(Some(env), env.fullName, Map()), interface)
      }
      case ImplEnvEntry(impl) => ImplTemplata(env, impl)
      case TemplataEnvEntry(templata) => templata
    }
  }

  def addEntries(
      oldEntries: Map[AbsoluteNameA[INameA], List[IEnvEntry]],
      newEntries: Map[AbsoluteNameA[INameA], List[IEnvEntry]]):
  Map[AbsoluteNameA[INameA], List[IEnvEntry]] = {
    oldEntries ++
      newEntries ++
      oldEntries.keySet.intersect(newEntries.keySet)
        .map(key => (key -> (oldEntries(key) ++ newEntries(key))))
        .toMap
  }

  def addEntry(
      oldEntries: Map[AbsoluteNameA[INameA], List[IEnvEntry]],
      name: AbsoluteNameA[INameA],
      entry: IEnvEntry):
  Map[AbsoluteNameA[INameA], List[IEnvEntry]] = {
    addEntries(oldEntries, Map(name -> List(entry)))
  }

  def addFunction(
    oldEntries: Map[AbsoluteNameA[INameA], List[IEnvEntry]],
    functionA: FunctionA
  ): Map[AbsoluteNameA[INameA], List[IEnvEntry]] = {
    addEntry(oldEntries, functionA.name, FunctionEnvEntry(functionA))
  }


  def entryMatchesFilter(entry: IEnvEntry, contexts: Set[ILookupContext]): Boolean = {
    entry match {
      case FunctionEnvEntry(_) => contexts.contains(ExpressionLookupContext)
      case ImplEnvEntry(_) => contexts.contains(ExpressionLookupContext)
      case StructEnvEntry(_) => contexts.contains(TemplataLookupContext)
      case InterfaceEnvEntry(_) => contexts.contains(TemplataLookupContext)
      case TemplataEnvEntry(templata) => {
        templata match {
          case CoordTemplata(_) => contexts.contains(TemplataLookupContext)
          case KindTemplata(_) => contexts.contains(TemplataLookupContext)
          case StructTemplata(_, _) => contexts.contains(TemplataLookupContext)
          case InterfaceTemplata(_, _) => contexts.contains(TemplataLookupContext)
          case ArrayTemplateTemplata() => contexts.contains(TemplataLookupContext)
          case BooleanTemplata(_) => true
          case FunctionTemplata(_, _, _) => contexts.contains(ExpressionLookupContext)
          case ImplTemplata(_, _) => contexts.contains(ExpressionLookupContext)
          case IntegerTemplata(_) => true
          case LocationTemplata(_) => contexts.contains(TemplataLookupContext)
          case MutabilityTemplata(_) => contexts.contains(TemplataLookupContext)
          case OwnershipTemplata(_) => contexts.contains(TemplataLookupContext)
          case PermissionTemplata(_) => contexts.contains(TemplataLookupContext)
          case VariabilityTemplata(_) => contexts.contains(TemplataLookupContext)
          case ExternImplTemplata(_, _) => contexts.contains(TemplataLookupContext)
          case ExternFunctionTemplata(_) => contexts.contains(ExpressionLookupContext)
        }
      }
    }
  }

  // See NTKPRR
  def assembleRulesFromFunctionAndContainers(containers: List[IContainer], function: FunctionA):
  (List[IRulexAR], Map[AbsoluteNameA[IRuneA], ITemplataType]) = {
    val (containersRules, containersTypeByRune) =
      containers
        .map({
          case ContainerInterface(interface) => (interface.rules, interface.typeByRune)
          case ContainerStruct(struct) => (struct.rules, struct.typeByRune)
          case ContainerFunction(function) => (function.templateRules, function.typeByRune)
          case ContainerImpl(impl) => vimpl()
        })
        .unzip
    val rules = containersRules.foldLeft(List[IRulexAR]())(_ ++ _)
    val typeByRune = containersTypeByRune.foldLeft(Map[AbsoluteNameA[IRuneA], ITemplataType]())(_ ++ _)
    (rules ++ function.templateRules, typeByRune ++ function.typeByRune)
  }

  // See OFCBT.
  def functionIsTemplateInContext(unevaluatedContainers: List[IContainer], function: FunctionA): Boolean = {
    function.isTemplate &&
    unevaluatedContainers.forall({
      case ContainerInterface(interface) => interface.isTemplate
      case ContainerStruct(struct) => struct.isTemplate
      case ContainerFunction(function) => function.isTemplate
      case ContainerImpl(impl) => vimpl()
    })
  }
}
