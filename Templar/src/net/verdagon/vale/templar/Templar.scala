package net.verdagon.vale.templar;

import net.verdagon.vale._
import net.verdagon.vale.astronomer._
import net.verdagon.vale.scout.CodeLocationS
import net.verdagon.vale.templar.OverloadTemplar.{ScoutExpectedFunctionFailure, ScoutExpectedFunctionSuccess}
import net.verdagon.vale.templar.citizen.StructTemplar
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar.function.{BuiltInFunctions, FunctionTemplar, FunctionTemplarCore, VirtualTemplar}

import scala.collection.immutable.{List, ListMap, Map, Set}

object Templar {
  def evaluate(program: ProgramA):
  CompleteProgram2 = {

    val ProgramA(structsA, interfacesA, impls1, functions1) = program;

    val env0 =
      NamespaceEnvironment(
        None,
        FullName2(List()),
        Map(
          "Int" -> List(TemplataEnvEntry(KindTemplata(Int2()))),
          "Array" -> List(TemplataEnvEntry(ArrayTemplateTemplata())),
          "Bool" -> List(TemplataEnvEntry(KindTemplata(Bool2()))),
          "Float" -> List(TemplataEnvEntry(KindTemplata(Float2()))),
          "__Never" -> List(TemplataEnvEntry(KindTemplata(Never2()))),
          "Str" -> List(TemplataEnvEntry(KindTemplata(Str2()))),
          "Void" -> List(TemplataEnvEntry(KindTemplata(Void2())))))
    val functionGeneratorByName0 = Map[String, IFunctionGenerator]()
    val (env1, functionGeneratorByName1) = BuiltInFunctions.addBuiltInFunctions(env0, functionGeneratorByName0)
    val functionGeneratorByName2 = functionGeneratorByName1 ++ StructTemplar.getFunctionGenerators()

    val env3 = env1

    // This has to come before the structs and interfaces because part of evaluating a
    // struct or interface is figuring out what it extends.
    val env5 =
      impls1.foldLeft(env3)({
        case (env4, impl1) => env4.addEntry(IMPL_NAME, ImplEnvEntry(impl1))
      })
    val env7 =
      structsA.foldLeft(env5)({
        case (env6, s) => env6.addEntries(makeStructEnvironmentEntries(s))
      })
    val env9 =
      interfacesA.foldLeft(env7)({
        case (env8, interfaceA) => env8.addEntries(makeInterfaceEnvironmentEntries(interfaceA))
      })

    val env11 =
      functions1.foldLeft(env9)({
        case (env10, functionS) => {
          env10.addFunction(functionS)
        }
      })

    val temputs =
      TemputsBox(
      Temputs(
        functionGeneratorByName2,
        Set(),
        Map(),
        List(),
        ListMap(),
        Map(),
        Set(),
        ListMap(),
        Map(),
        Set(),
        ListMap(),
        Map(),
        List(),
        Map(),
        Map(),
        Map()))

    val emptyPackStructRef = StructTemplar.addBuiltInStructs(env11, temputs)

      structsA.foreach({
        case (structS @ StructA(_, _, _, _, _, _, _, _)) => {
          if (structS.isTemplate) {
            // Do nothing, it's a template
          } else {
            val structTemplata = StructTemplata(env11, structS)
            val _ = StructTemplar.getStructRef(temputs, structTemplata, List())
          }
        }
      })

      interfacesA.foreach({
        case (interfaceS @ InterfaceA(_, _, _, _, _, _, _, _)) => {
          if (interfaceS.isTemplate) {
            // Do nothing, it's a template
          } else {
            val _ =
              StructTemplar.getInterfaceRef(
                temputs, InterfaceTemplata(env11, interfaceS), List())
          }
        }
      })

      functions1.foreach({
        case (functionS) => {
          if (functionS.isTemplate) {
            // Do nothing, it's a template
          } else {
            println("fill in these containers!")
            val _ =
              FunctionTemplar.evaluateOrdinaryFunctionFromNonCallForPrototype(
                temputs, FunctionTemplata(env11, List(), functionS))
          }
        }
      })

    stampNeededOverridesUntilSettled(env11, temputs)

    val result =
      CompleteProgram2(
        temputs.getAllInterfaces().toList,
        temputs.getAllStructs().toList,
        temputs.impls,
        emptyPackStructRef,
        temputs.getAllFunctions())

    result
  }

  // (Once we add namespaces, this will probably change)
  def makeInterfaceEnvironmentEntries(
    interfaceA: InterfaceA
  ): Map[String, List[IEnvEntry]] = {
    val interfaceEnvEntry = InterfaceEnvEntry(interfaceA)

    val env0 = Map[String, List[IEnvEntry]]()
    val env1 = EnvironmentUtils.addEntry(env0, interfaceA.name, interfaceEnvEntry)
    val env2 = EnvironmentUtils.addFunction(env1, StructTemplar.getInterfaceConstructor(interfaceA))

    val env4 =
      interfaceA.internalMethods.foldLeft(env2)({
        case (env3, internalMethodA) => EnvironmentUtils.addFunction(env3, internalMethodA)
      })

    // Once we have sub-interfaces and sub-structs, we could recursively call this function.
    // We'll put our interfaceA onto the top of the list of every entry from the sub-struct/sub-interface.

    env4
  }

  // (Once we add namespaces, this will probably change)
  def makeStructEnvironmentEntries(
    structA: StructA
  ): Map[String, List[IEnvEntry]] = {
    val interfaceEnvEntry = StructEnvEntry(structA)

    val env0 = Map[String, List[IEnvEntry]]()
    val env1 = EnvironmentUtils.addEntry(env0, structA.name, interfaceEnvEntry)
    val env2 = EnvironmentUtils.addFunction(env1, StructTemplar.getConstructor(structA))

    // To add once we have methods inside structs:
//    val env4 =
//      structA.internalMethods.foldLeft(env2)({
//        case (env3, internalMethodA) => EnvironmentUtils.addFunction(env3, Some(interfaceEnvEntry), internalMethodA)
//      })

    // Once we have sub-interfaces and sub-structs, we could recursively call this function.
    // We'll put our structA onto the top of the list of every entry from the sub-struct/sub-interface.

    env2
  }

  def stampNeededOverridesUntilSettled(env: NamespaceEnvironment[IName2], temputs: TemputsBox): Unit = {
    val neededOverrides = EdgeTemplar.assembleEdges(temputs.functions, temputs.getAllInterfaces(), temputs.impls)

    if (neededOverrides.isEmpty) {
      return temputs
    }

    // right now we're just assuming global env, but it might not be there...
    // perhaps look in the struct's env and the function's env? cant think of where else to look.
    println("which envs do we look in?")

      neededOverrides.foreach({
        case (neededOverride) => {
            OverloadTemplar.scoutExpectedFunctionForPrototype(
              env,
              temputs,
              neededOverride.name,
              List(), // No explicitly specified ones. It has to be findable just by param filters.
              neededOverride.paramFilters,
              true) match {
            case (seff @ ScoutExpectedFunctionFailure(_, _, _, _, _)) => {
              vfail("Couldn't find function for vtable!\n" + seff.toString)
            }
            case (ScoutExpectedFunctionSuccess(_)) =>
          }
        }
      })

    stampNeededOverridesUntilSettled(env, temputs)
  }

  def getMutabilities(temputs: TemputsBox, concreteValues2: List[Kind]):
  List[Mutability] = {
    concreteValues2.map(concreteValue2 => getMutability(temputs, concreteValue2))
  }

  def getMutability(temputs: TemputsBox, concreteValue2: Kind):
  Mutability = {
    concreteValue2 match {
      case Never2() => Immutable
      case Int2() => Immutable
      case Float2() => Immutable
      case Bool2() => Immutable
      case Str2() => Immutable
      case Void2() => Immutable
      case UnknownSizeArrayT2(RawArrayT2(_, mutability)) => mutability
      case ArraySequenceT2(_, RawArrayT2(_, mutability)) => mutability
      case sr @ StructRef2(_) => temputs.lookupMutability(sr)
      case ir @ InterfaceRef2(_) => temputs.lookupMutability(ir)
      case PackT2(_, sr) => temputs.lookupMutability(sr)
      case OverloadSet(_, _, _) => {
        // Just like FunctionT2
        Immutable
      }
    }
  }

  def runTemplar(code: String): Option[CompleteProgram2] = {
    Astronomer.runAstronomer(code) match {
      case None => None
      case Some(program1) => Some(evaluate(program1))
    }
  }
}
