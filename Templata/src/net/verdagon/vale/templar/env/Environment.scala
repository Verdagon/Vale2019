package net.verdagon.vale.templar.env

import net.verdagon.vale.astronomer._
import net.verdagon.vale.scout.{Environment => _, FunctionEnvironment => _, IEnvironment => _, _}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.{vassert, vfail, vimpl, vwat}

import scala.collection.immutable.{List, Map}

trait IEnvironment {
  override def toString: String = {
    "#Environment"
  }
  def globalEnv: NamespaceEnvironment[IName2]
  def getAllTemplatasWithAbsoluteName2(name: IName2, lookupFilter: Set[ILookupContext]): List[ITemplata]
  def getNearestTemplataWithAbsoluteName2(name: IName2, lookupFilter: Set[ILookupContext]): Option[ITemplata]
  def getAllTemplatasWithAbsoluteNameA(name: INameA, lookupFilter: Set[ILookupContext]): List[ITemplata]
  def getNearestTemplataWithAbsoluteNameA(name: INameA, lookupFilter: Set[ILookupContext]): Option[ITemplata]
  def getAllTemplatasWithName(name: IImpreciseNameStepA, lookupFilter: Set[ILookupContext]): List[ITemplata]
  def getNearestTemplataWithName(name: IImpreciseNameStepA, lookupFilter: Set[ILookupContext]): Option[ITemplata]
  def fullName: FullName2[IName2]
}

trait IEnvironmentBox {
  def snapshot: IEnvironment
  override def toString: String = {
    "#Environment"
  }
  def globalEnv: NamespaceEnvironment[IName2]
  def getAllTemplatasWithAbsoluteNameA(name: INameA, lookupFilter: Set[ILookupContext]): List[ITemplata]
  def getNearestTemplataWithAbsoluteNameA(name: INameA, lookupFilter: Set[ILookupContext]): Option[ITemplata]
  def getAllTemplatasWithName(name: IImpreciseNameStepA, lookupFilter: Set[ILookupContext]): List[ITemplata]
  def getNearestTemplataWithName(name: IImpreciseNameStepA, lookupFilter: Set[ILookupContext]): Option[ITemplata]
  def fullName: FullName2[IName2]
}

sealed trait ILookupContext
case object TemplataLookupContext extends ILookupContext
case object ExpressionLookupContext extends ILookupContext

case class NamespaceEnvironment[+T <: IName2](
  maybeParentEnv: Option[IEnvironment],
  fullName: FullName2[T],
  entries: Map[IName2, List[IEnvEntry]]
) extends IEnvironment {
  maybeParentEnv match {
    case None =>
    case Some(parentEnv) => vassert(fullName.steps.startsWith(parentEnv.fullName.steps))
  }

  override def globalEnv: NamespaceEnvironment[IName2] = {
    maybeParentEnv match {
      case None => this
      case Some(parentEnv) => parentEnv.globalEnv
    }
  }

  override def getAllTemplatasWithAbsoluteNameA(
    name: INameA,
    lookupFilter: Set[ILookupContext]):
  List[ITemplata] = {
    entries
      .filter({ case (key, _) => EnvironmentUtils.namesMatch(name, key) })
      .values
      .flatten
      .filter(EnvironmentUtils.entryMatchesFilter(_, lookupFilter))
      .toList
      .map(EnvironmentUtils.entryToTemplata(this, _)) ++
      maybeParentEnv.toList.flatMap(_.getAllTemplatasWithAbsoluteNameA(name, lookupFilter))
  }

  override def getNearestTemplataWithAbsoluteNameA(name: INameA, lookupFilter: Set[ILookupContext]): Option[ITemplata] = {
    entries
      .filter({ case (key, _) => EnvironmentUtils.namesMatch(name, key) })
      .values
      .flatten
      .filter(EnvironmentUtils.entryMatchesFilter(_, lookupFilter)) match {
      case List(entry) => Some(EnvironmentUtils.entryToTemplata(this, entry))
      case List() => {
        maybeParentEnv match {
          case None => None
          case Some(parentEnv) => parentEnv.getNearestTemplataWithAbsoluteNameA(name, lookupFilter)
        }
      }
      case multiple => vfail("Too many things named " + name + ":\n" + multiple.mkString("\n"));
    }
  }

  override def getAllTemplatasWithAbsoluteName2(name: IName2, lookupFilter: Set[ILookupContext]): List[ITemplata] = {
    vimpl()
  }

  override def getAllTemplatasWithName(name: IImpreciseNameStepA, lookupFilter: Set[ILookupContext]): List[ITemplata] = {
    vimpl()
  }

  override def getNearestTemplataWithAbsoluteName2(name: IName2, lookupFilter: Set[ILookupContext]): Option[ITemplata] = {
    vimpl()
  }

  override def getNearestTemplataWithName(name: IImpreciseNameStepA, lookupFilter: Set[ILookupContext]): Option[ITemplata] = {
    vimpl()
  }

  def addFunction(function: FunctionA): NamespaceEnvironment[T] = {
    NamespaceEnvironment(
      maybeParentEnv,
      fullName,
      EnvironmentUtils.addFunction(entries, function))
  }

  def addEntry(name: IName2, entry: IEnvEntry): NamespaceEnvironment[T] = {
    NamespaceEnvironment(
      maybeParentEnv,
      fullName,
      EnvironmentUtils.addEntry(entries, name, entry))
  }

  def addEntries(newEntries: Map[IName2, List[IEnvEntry]]): NamespaceEnvironment[T] = {
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
      oldEntries: Map[IName2, List[IEnvEntry]],
      newEntries: Map[IName2, List[IEnvEntry]]):
  Map[IName2, List[IEnvEntry]] = {
    oldEntries ++
      newEntries ++
      oldEntries.keySet.intersect(newEntries.keySet)
        .map(key => (key -> (oldEntries(key) ++ newEntries(key))))
        .toMap
  }

  def addEntry(
      oldEntries: Map[IName2, List[IEnvEntry]],
      name: IName2,
      entry: IEnvEntry):
  Map[IName2, List[IEnvEntry]] = {
    addEntries(oldEntries, Map(name -> List(entry)))
  }

  def addFunction(
    oldEntries: Map[IName2, List[IEnvEntry]],
    functionA: FunctionA
  ): Map[IName2, List[IEnvEntry]] = {
    val name = start here is this a template? vimpl()//functionA.name
    addEntry(oldEntries, name, FunctionEnvEntry(functionA))
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
  (List[IRulexAR], Map[IRuneA, ITemplataType]) = {
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
    val typeByRune = containersTypeByRune.foldLeft(Map[IRuneA, ITemplataType]())(_ ++ _)
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

  def namesMatch(nameA: INameA, name2: IName2): Boolean = {
    (nameA, name2) match {
      case (LambdaNameA(parent, codeLocationA), LambdaName2(codeLocation2, templateArgs2, parameters2)) => vimpl()//codeLocationsMatch(codeLocationA, codeLocation2)
      case (FunctionNameA(nameA, codeLocationA), FunctionName2(name2, templateArgs2, codeLocation2)) => vimpl()//nameA == name2 && codeLocationsMatch(codeLocationA, codeLocation2)
      case (TopLevelCitizenDeclarationNameA(nameA, codeLocationA), StructName2(name2, templateArgs)) => vimpl()//nameA == name2 && codeLocationsMatch(codeLocationA, codeLocation2)
      case (TopLevelCitizenDeclarationNameA(nameA, codeLocationA), InterfaceName2(name2, templateArgs)) => vimpl()//nameA == name2 && codeLocationsMatch(codeLocationA, codeLocation2)
      case (LambdaStructNameA(_), LambdaStructName2(codeLocation2)) => vimpl()//codeLocationsMatch(codeLocationA, codeLocation2)
      case (ImplNameA(codeLocationA), ImplDeclareName2(codeLocation2)) => codeLocationsMatch(codeLocationA, codeLocation2)
      case (LetNameA(codeLocationA), LetName2(codeLocation2)) => codeLocationsMatch(codeLocationA, codeLocation2)
      case (UnnamedLocalNameA(codeLocationA), UnnamedLocalName2(codeLocation2)) => codeLocationsMatch(codeLocationA, codeLocation2)
      case (ClosureParamNameA(), ClosureParamName2()) => true
      case (MagicParamNameA(magicParamNumberA), MagicParamName2(magicParamNumber2)) => magicParamNumberA == magicParamNumber2
      case (CodeVarNameA(nameA), CodeVarName2(name2)) => nameA == name2
    }
  }

//  def runesMatch(runeA: IRuneA, rune2: IRune2): Boolean = {
//    (runeA, rune2) match {
//      case (CodeRuneA(nameA), CodeRune2(name2)) => nameA == name2
//      case (ImplicitRuneA(nameA), ImplicitRune2(name2)) => nameA == name2
//      case (MemberRuneA(memberIndexA), MemberRune2(memberIndex2)) => memberIndexA == memberIndex2
//      case (MagicImplicitRuneA(magicParamIndexA), MagicImplicitRune2(magicParamIndex2)) => magicParamIndexA == magicParamIndex2
//      case (ReturnRuneA(), ReturnRune2()) => true
//    }
//  }

  def codeLocationsMatch(codeLocationA: CodeLocationS, codeLocation2: CodeLocation2): Boolean = {
    val CodeLocationS(lineS, charS) = codeLocationA
    val CodeLocation2(line2, char2) = codeLocation2
    lineS == line2 && charS == char2
  }
}
