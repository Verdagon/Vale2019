package net.verdagon.vale.templar.env

import net.verdagon.vale.astronomer.FunctionA
import net.verdagon.vale.scout.{IEnvironment => _, FunctionEnvironment => _, Environment => _, _}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar.types.{FullName2, NamePart2}
import net.verdagon.vale.{vassert, vfail}

import scala.collection.immutable.{List, Map}

trait IEnvironment {
  override def toString: String = {
    "#Environment"
  }
  def globalEnv: NamespaceEnvironment
  def getAllTemplatasWithName(name: String, lookupFilter: Set[ILookupContext]): List[ITemplata]
  def getNearestTemplataWithName(name: String, lookupFilter: Set[ILookupContext]): Option[ITemplata]
  def fullName: FullName2
}

trait IEnvironmentBox {
  def snapshot: IEnvironment
  override def toString: String = {
    "#Environment"
  }
  def globalEnv: NamespaceEnvironment
  def getAllTemplatasWithName(name: String, lookupFilter: Set[ILookupContext]): List[ITemplata]
  def getNearestTemplataWithName(name: String, lookupFilter: Set[ILookupContext]): Option[ITemplata]
  def fullName: FullName2
}

sealed trait ILookupContext
case object TemplataLookupContext extends ILookupContext
case object ExpressionLookupContext extends ILookupContext

case class NamespaceEnvironment(
  maybeParentEnv: Option[IEnvironment],
  fullName: FullName2,
  entries: Map[String, List[IEnvEntry]]
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

  override def getAllTemplatasWithName(name: String, lookupFilter: Set[ILookupContext]): List[ITemplata] = {
    entries.getOrElse(name, List())
      .filter(EnvironmentUtils.entryMatchesFilter(_, lookupFilter))
      .map(EnvironmentUtils.entryToTemplata(this, _))++
      maybeParentEnv.toList.flatMap(_.getAllTemplatasWithName(name, lookupFilter))
  }

  override def getNearestTemplataWithName(name: String, lookupFilter: Set[ILookupContext]): Option[ITemplata] = {
    entries
      .get(name).toList.flatten
      .filter(EnvironmentUtils.entryMatchesFilter(_, lookupFilter)) match {
      case List(entry) => Some(EnvironmentUtils.entryToTemplata(this, entry))
      case List() => {
        maybeParentEnv match {
          case None => None
          case Some(parentEnv) => parentEnv.getNearestTemplataWithName(name, lookupFilter)
        }
      }
      case multiple => vfail("Too many things named " + name + ":\n" + multiple.mkString("\n"));
    }
  }


  def addFunction(
    parent: Option[IEnvEntry],
    function: FunctionA
  ): NamespaceEnvironment = {
    NamespaceEnvironment(
      maybeParentEnv,
      fullName,
      EnvironmentUtils.addFunction(entries, parent, function))
  }

  def addEntry(name: String, entry: IEnvEntry): NamespaceEnvironment = {
    NamespaceEnvironment(
      maybeParentEnv,
      fullName,
      EnvironmentUtils.addEntry(entries, name, entry))
  }

  def addEntries(newEntries: Map[String, List[IEnvEntry]]): NamespaceEnvironment = {
    NamespaceEnvironment(
      maybeParentEnv,
      fullName,
      EnvironmentUtils.addEntries(entries, newEntries))
  }
}

object EnvironmentUtils {
  def entryToTemplata(env: IEnvironment, entry: IEnvEntry): ITemplata = {
    entry match {
      case FunctionEnvEntry(_, function) => FunctionTemplata(env, function)
      case StructEnvEntry(_, struct) => {
        StructTemplata(NamespaceEnvironment(Some(env), env.fullName, Map()), struct)
      }
      case InterfaceEnvEntry(_, interface) => {
        InterfaceTemplata(NamespaceEnvironment(Some(env), env.fullName, Map()), interface)
      }
      case ImplEnvEntry(_, impl) => ImplTemplata(env, impl)
      case TemplataEnvEntry(templata) => templata
    }
  }

  def addEntries(
      oldEntries: Map[String, List[IEnvEntry]],
      newEntries: Map[String, List[IEnvEntry]]):
  Map[String, List[IEnvEntry]] = {
    oldEntries ++
      newEntries ++
      oldEntries.keySet.intersect(newEntries.keySet)
        .map(key => (key -> (oldEntries(key) ++ newEntries(key))))
        .toMap
  }

  def addEntry(
      oldEntries: Map[String, List[IEnvEntry]],
      name: String,
      entry: IEnvEntry):
  Map[String, List[IEnvEntry]] = {
    addEntries(oldEntries, Map(name -> List(entry)))
  }

  def addFunction(
    oldEntries: Map[String, List[IEnvEntry]],
    parent: Option[IEnvEntry],
    functionA: FunctionA
  ): Map[String, List[IEnvEntry]] = {
    addEntry(oldEntries, functionA.name, FunctionEnvEntry(parent, functionA))
  }


  def entryMatchesFilter(entry: IEnvEntry, contexts: Set[ILookupContext]): Boolean = {
    entry match {
      case FunctionEnvEntry(_, _) => contexts.contains(ExpressionLookupContext)
      case ImplEnvEntry(_, _) => contexts.contains(ExpressionLookupContext)
      case StructEnvEntry(_, _) => contexts.contains(TemplataLookupContext)
      case InterfaceEnvEntry(_, _) => contexts.contains(TemplataLookupContext)
      case TemplataEnvEntry(templata) => {
        templata match {
          case CoordTemplata(_) => contexts.contains(TemplataLookupContext)
          case KindTemplata(_) => contexts.contains(TemplataLookupContext)
          case StructTemplata(_, _) => contexts.contains(TemplataLookupContext)
          case InterfaceTemplata(_, _) => contexts.contains(TemplataLookupContext)
          case ArrayTemplateTemplata() => contexts.contains(TemplataLookupContext)
          case BooleanTemplata(_) => true
          case FunctionTemplata(_, _) => contexts.contains(ExpressionLookupContext)
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
}
