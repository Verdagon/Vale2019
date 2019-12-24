package net.verdagon.vale.templar;

import net.verdagon.vale._
import net.verdagon.vale.astronomer._
import net.verdagon.vale.parser._
import net.verdagon.vale.scout._
import net.verdagon.vale.scout.patterns.{AbstractSP, AtomSP}
import net.verdagon.vale.scout.rules._
import net.verdagon.vale.templar.OverloadTemplar.{ScoutExpectedFunctionFailure, ScoutExpectedFunctionSuccess}
import net.verdagon.vale.templar.citizen.StructTemplar
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.types._
import net.verdagon.vale.templar.templata._
import net.verdagon.vale.templar.function.{BuiltInFunctions, FunctionTemplar, FunctionTemplarCore, VirtualTemplar}

import scala.collection.immutable.{List, ListMap, Map, Set}

object Templar {
  val IMPL_NAME = "__impl"

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

    val env2 =
      env1
        .addEntry(
          "IFunction1",
          InterfaceEnvEntry(
            InterfaceA(
              CodeLocation("IFunction1.builtin.vale", 0, 0),
              List(),
              "IFunction1",
              MutableP,
              Some(MutableP),
              TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType, CoordTemplataType), KindTemplataType),
              List("M", "P1", "R"),
              Map(
                "M" -> MutabilityTemplataType,
                "P1" -> CoordTemplataType,
                "R" -> CoordTemplataType),
              List(
                TemplexAR(RuneAT("M", MutabilityTemplataType)),
                TemplexAR(RuneAT("P1", CoordTemplataType)),
                TemplexAR(RuneAT("R", CoordTemplataType))))))
          .addFunction(
            FunctionA(
              CodeLocation("IFunction1.builtin.vale", 0, 1),
              "__call", List(), 0, true,
              TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType, CoordTemplataType), FunctionTemplataType),
              List("M", "P1", "R"),
              Map(
                "M" -> MutabilityTemplataType,
                "P1" -> CoordTemplataType,
                "R" -> CoordTemplataType,
                "ThisK" -> CoordTemplataType),
              List(
                ParameterS(AtomSP(Some(CaptureP("this", FinalP)), Some(AbstractSP), "BorrowThis", None)),
                ParameterS(AtomSP(Some(CaptureP("p1", FinalP)), None, "P1", None))),
              Some("R"),
              List(
                EqualsAR(
                  TemplexAR(RuneAT("ThisK", CoordTemplataType)),
                  TemplexAR(
                    CallAT(
                      NameAT("IFunction1", TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType, CoordTemplataType), KindTemplataType)),
                      List(
                        RuneAT("M", MutabilityTemplataType),
                        RuneAT("P1", CoordTemplataType),
                        RuneAT("R", CoordTemplataType)),
                      CoordTemplataType))),
                EqualsAR(
                  TemplexAR(RuneAT("BorrowThis", CoordTemplataType)),
                  TemplexAR(OwnershippedAT(BorrowP, RuneAT("ThisK", CoordTemplataType))))),
              AbstractBodyA))

    // This has to come before the structs and interfaces because part of evaluating a
    // struct or interface is figuring out what it extends.
    val env4 =
      impls1.foldLeft(env2)({
        case (envH, impl1) => envH.addEntry(IMPL_NAME, ImplEnvEntry(impl1))
      })
    val env5 =
      structsA.foldLeft(env4)({
        case (env4a, s) => {
          env4a
            .addEntry(s.name, StructEnvEntry(s))
            .addFunction(StructTemplar.getConstructor(s))
        }
      })
    val env7 =
      interfacesA.foldLeft(env5)({
        case (env6, i) => env6.addEntry(i.name, InterfaceEnvEntry(i))
      })

//    val env9 =
//      envExternFunctionMembers.foldLeft(env6)({
//        case (env8, (name, headers)) => {
//          env8.addEntries(
//            Map((name -> headers.map(header => TemplataEnvEntry(ExternFunctionTemplata(header))))))
//        }
//      })
    val env9 = env7

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
        case (structS @ StructA(_, _, _, _, _, _, _, _, _, _)) => {
          if (structS.isTemplate) {
            // Do nothing, it's a template

          } else {
            val _ = StructTemplar.getStructRef(env11, temputs, structS, List())
          }
        }
      })

      interfacesA.foreach({
        case (interfaceS @ InterfaceA(_, _, _, _, _, _, _, _, _)) => {
          if (interfaceS.isTemplate) {
            // Do nothing, it's a template
          } else {
            val _ = StructTemplar.getInterfaceRef(env11, temputs, interfaceS, List())
          }
        }
      })

//
//      envExternFunctionMembers.values.foldLeft(temputs)({
//        case (headers) => {
//          headers.foldLeft(temputs)({
//            case (header) => {
//              temputs.declareFunctionSignature(header.toSignature, None)
//              val _ =
//                FunctionTemplarCore.makeExternFunction(
//                  temputs,
//                  header.toBanner.humanName,
//                  header.isUserFunction,
//                  header.templateArgs,
//                  header.params,
//                  header.returnType,
//                  None)
//
//              vassert(temputs.exactDeclaredSignatureExists(header.toBanner.humanName, header.toBanner.templateArgs, header.toBanner.paramTypes))
//
//              temputs
//            }
//          })
//        }
//      })

      functions1.foreach({
        case (functionS) => {
          if (functionS.isTemplate) {
            // Do nothing, it's a template
          } else {
            val _ =
              FunctionTemplar.evaluateOrdinaryFunctionFromNonCallForPrototype(
                temputs, FunctionTemplata(env11, functionS))
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

  def stampNeededOverridesUntilSettled(env: NamespaceEnvironment, temputs: TemputsBox): Unit = {
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

  def runTemplar(programA: ProgramA): CompleteProgram2 = {
    Templar.evaluate(programA)
  }

  def runTemplar(code: String): Option[CompleteProgram2] = {
    Astronomer.runAstronomer(code) match {
      case None => None
      case Some(program1) => Some(runTemplar(program1))
    }
  }
}
