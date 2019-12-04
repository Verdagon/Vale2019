package net.verdagon.radonc.templar;

import net.verdagon.radonc._
import net.verdagon.radonc.astronomer._
import net.verdagon.radonc.parser._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.{AbstractSP, AtomSP}
import net.verdagon.radonc.scout.rules._
import net.verdagon.radonc.templar.OverloadTemplar.{ScoutExpectedFunctionFailure, ScoutExpectedFunctionSuccess}
import net.verdagon.radonc.templar.citizen.StructTemplar
import net.verdagon.radonc.templar.env._
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.templar.function.{BuiltInFunctions, FunctionTemplar, FunctionTemplarCore, VirtualTemplar}

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
          "__Array" -> List(TemplataEnvEntry(ArrayTemplateTemplata())),
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
        case (env3, impl1) => env3.addEntry(IMPL_NAME, ImplEnvEntry(impl1))
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

    val temputs0 =
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
        Map())

    val (temputs1, emptyPackStructRef) =
      StructTemplar.addBuiltInStructs(env11, temputs0)

    val temputs2 =
      structsA.foldLeft(temputs1)({
        case (temputs1b, structS @ StructA(_, _, _, _, _, _, _, _, _, _)) => {
          if (structS.isTemplate) {
            // Do nothing, it's a template
            temputs1b
          } else {
            val (temputs1c, _) =
              StructTemplar.getStructRef(env11, temputs1b, structS, List())
            temputs1c
          }
        }
      })

    val temputs3 =
      interfacesA.foldLeft(temputs2)({
        case (temputs2b, interfaceS @ InterfaceA(_, _, _, _, _, _, _, _, _)) => {
          if (interfaceS.isTemplate) {
            // Do nothing, it's a template
            temputs2b
          } else {
            val (temputs2c, _) =
              StructTemplar.getInterfaceRef(env11, temputs2b, interfaceS, List())
            temputs2c
          }
        }
      })

    val temputs10 = temputs3


//    val temputs15 =
//      envExternFunctionMembers.values.foldLeft(temputs10)({
//        case (temputs11, headers) => {
//          headers.foldLeft(temputs11)({
//            case (temputs12, header) => {
//              val temputs13 = temputs12.declareFunctionSignature(header.toSignature, None)
//              val (temputs14, _) =
//                FunctionTemplarCore.makeExternFunction(
//                  temputs13,
//                  header.toBanner.humanName,
//                  header.isUserFunction,
//                  header.templateArgs,
//                  header.params,
//                  header.returnType,
//                  None)
//
//              vassert(temputs13.exactDeclaredSignatureExists(header.toBanner.humanName, header.toBanner.templateArgs, header.toBanner.paramTypes))
//
//              temputs14
//            }
//          })
//        }
//      })
    val temputs15 = temputs10

    val temputs16 =
      functions1.foldLeft(temputs15)({
        case (temputs14, functionS) => {
          if (functionS.isTemplate) {
            // Do nothing, it's a template
            temputs14
          } else {
            val (temputs15, _) =
              FunctionTemplar.evaluateOrdinaryFunctionFromNonCallForPrototype(
                temputs14, FunctionTemplata(env11, functionS))
            temputs15
          }
        }
      })

    val temputs17 = stampNeededOverridesUntilSettled(env11, temputs16)

    val result =
      CompleteProgram2(
        temputs17.getAllInterfaces().toList,
        temputs17.getAllStructs().toList,
        temputs17.impls,
        emptyPackStructRef,
        temputs17.getAllFunctions())

    result
  }

  def stampNeededOverridesUntilSettled(env: NamespaceEnvironment, temputs0: Temputs): Temputs = {
    val neededOverrides = EdgeTemplar.assembleEdges(temputs0.functions, temputs0.getAllInterfaces(), temputs0.impls)

    if (neededOverrides.isEmpty) {
      return temputs0
    }

    // right now we're just assuming global env, but it might not be there...
    // perhaps look in the struct's env and the function's env? cant think of where else to look.
    println("which envs do we look in?")

    val temputs3 =
      neededOverrides.foldLeft(temputs0)({
        case (temputs1, neededOverride) => {
          val temputs2 =
            OverloadTemplar.scoutExpectedFunctionForPrototype(
              env,
              temputs1,
              neededOverride.name,
              List(), // No explicitly specified ones. It has to be findable just by param filters.
              neededOverride.paramFilters,
              true) match {
            case (_, seff @ ScoutExpectedFunctionFailure(_, _, _, _, _)) => {
              vfail("Couldn't find function for vtable!\n" + seff.toString)
            }
            case (temputs1b, ScoutExpectedFunctionSuccess(_)) => temputs1b
          }
          temputs2
        }
      })

    stampNeededOverridesUntilSettled(env, temputs3)
  }

  def getMutabilities(temputs0: Temputs, concreteValues2: List[Kind]):
  List[Mutability] = {
    concreteValues2.map(concreteValue2 => getMutability(temputs0, concreteValue2))
  }

  def getMutability(temputs0: Temputs, concreteValue2: Kind):
  Mutability = {
    concreteValue2 match {
      case Never2() => Immutable
      case Int2() => Immutable
      case Float2() => Immutable
      case Bool2() => Immutable
      case Str2() => Immutable
      case Void2() => Immutable
      case FunctionT2(_, _) => Immutable
      case UnknownSizeArrayT2(RawArrayT2(_, mutability)) => mutability
      case ArraySequenceT2(_, RawArrayT2(_, mutability)) => mutability
      case sr @ StructRef2(_) => temputs0.lookupMutability(sr)
      case ir @ InterfaceRef2(_) => temputs0.lookupMutability(ir)
      case PackT2(_, sr) => temputs0.lookupMutability(sr)
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
