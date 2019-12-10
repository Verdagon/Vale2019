package net.verdagon.radonc.templar.function

import net.verdagon.radonc.astronomer._
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.parser._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.{AbstractSP, AtomSP, OverrideSP}
import net.verdagon.radonc.scout.rules._
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.env.{FunctionEnvironment, _}
import net.verdagon.radonc.{vassert, vfail}

import scala.collection.immutable.List

object BuiltInFunctions {
  def addBuiltInFunctions(
    currentlyConstructingEnv0: NamespaceEnvironment,
    functionGeneratorByName0: Map[String, IFunctionGenerator]):
  (NamespaceEnvironment, Map[String, IFunctionGenerator]) = {
    val (currentlyConstructingEnv1, functionGeneratorByName1) = addConcreteDestructor(currentlyConstructingEnv0, functionGeneratorByName0)
    val (currentlyConstructingEnv2, functionGeneratorByName2) = addInterfaceDestructor(currentlyConstructingEnv1, functionGeneratorByName1)
    val (currentlyConstructingEnv3, functionGeneratorByName3) = addImplDestructor(currentlyConstructingEnv2, functionGeneratorByName2)
    val (currentlyConstructingEnv4, functionGeneratorByName4) = addDrop(currentlyConstructingEnv3, functionGeneratorByName3)
    val currentlyConstructingEnv5 = addArrayLen(currentlyConstructingEnv4)
    val currentlyConstructingEnv6 = addPanic(currentlyConstructingEnv5)
    (currentlyConstructingEnv6, functionGeneratorByName4)
  }

  private def addConcreteDestructor(
    currentlyConstructingEnv: NamespaceEnvironment,
    functionGeneratorByName: Map[String, IFunctionGenerator]
  ): (NamespaceEnvironment, Map[String, IFunctionGenerator]) = {
    // Note the virtuality None in the header, and how we filter so this only applies
    // to structs and not interfaces. We use a different template for interface destructors.
    (
    currentlyConstructingEnv
      .addEntry(
        CallTemplar.DESTRUCTOR_NAME,
        FunctionEnvEntry(
          FunctionA(
            CodeLocation(CallTemplar.DESTRUCTOR_NAME + ".builtin.vale", 0, 0),
            CallTemplar.DESTRUCTOR_NAME,
            List(),
            0,
            true,
            TemplateTemplataType(List(CoordTemplataType), FunctionTemplataType),
            List("T"),
            Map("T" -> CoordTemplataType),
            List(
              ParameterS(AtomSP(Some(CaptureP("this", FinalP)), None, "T", None))),
            Some("V"),
            List(
              EqualsAR(
                TemplexAR(RuneAT("T", CoordTemplataType)),
                ComponentsAR(
                  CoordTemplataType,
                  List(
                    OrAR(List(TemplexAR(OwnershipAT(OwnP)), TemplexAR(OwnershipAT(ShareP)))),
                    CallAR(
                      "passThroughIfConcrete",
                      List(TemplexAR(AnonymousRuneAT(KindTemplataType))),
                      KindTemplataType)))),
              EqualsAR(
                TemplexAR(RuneAT("V", CoordTemplataType)),
                TemplexAR(NameAT("Void", CoordTemplataType)))),
            GeneratedBodyA("concreteDestructorGenerator")))),
      functionGeneratorByName +
        (
        "concreteDestructorGenerator" ->
          new IFunctionGenerator {
            override def generate(
              env: FunctionEnvironmentBox,
              temputs: TemputsBox,
              maybeOriginFunction1: Option[FunctionA],
              paramCoords: List[Parameter2],
              maybeReturnType2: Option[Coord]):
            (FunctionHeader2) = {
              // Even though below we treat packs, closures, and structs the same, they're
              // still disambiguated by the template arguments.
              paramCoords.map(_.tyype) match {
                case List(Coord(_, PackT2(_, structRef))) => {
                  DestructorTemplar.generateStructDestructor(
                    env, temputs, maybeOriginFunction1.get, paramCoords, structRef)
                }
  //              case List(Coord(_, OrdinaryClosure2(_, structRef2, _))) => {
  //                DestructorTemplar.generateStructDestructor(
  //                  env, temputs, maybeOriginFunction1, templateArgTemplatas, paramCoords, structRef2)
  //              }
  //              case List(Coord(_, TemplatedClosure2(_, structRef2, _))) => {
  //                DestructorTemplar.generateStructDestructor(
  //                  env, temputs, maybeOriginFunction1, templateArgTemplatas, paramCoords, structRef2)
  //              }
                case List(Coord(_, sr @ StructRef2(_))) => {
                  DestructorTemplar.generateStructDestructor(
                    env, temputs, maybeOriginFunction1.get, paramCoords, sr)
                }
                case List(r @ Coord(_, as @ ArraySequenceT2(_, _))) => {
                  DestructorTemplar.generateArraySequenceDestructor(
                    env.snapshot, temputs, maybeOriginFunction1, r, as)
                }
                case List(r @ Coord(_, ra @ UnknownSizeArrayT2(_))) => {
                  DestructorTemplar.generateUnknownSizeArrayDestructor(
                    env.snapshot, temputs, maybeOriginFunction1, r, ra)
                }
                case _ => {
                  vfail("wot")
                }
              }
            }
          }))
  }

  private def addInterfaceDestructor(
    currentlyConstructingEnv: NamespaceEnvironment,
    functionGeneratorByName: Map[String, IFunctionGenerator]
  ): (NamespaceEnvironment, Map[String, IFunctionGenerator]) = {

    (
    currentlyConstructingEnv
      .addFunction(
        FunctionA(
          CodeLocation(CallTemplar.INTERFACE_DESTRUCTOR_NAME + ".builtin.vale", 0, 0),
          CallTemplar.INTERFACE_DESTRUCTOR_NAME,
          List(),
          0,
          true,
          TemplateTemplataType(List(CoordTemplataType), FunctionTemplataType),
          List("T"),
          Map("T" -> CoordTemplataType),
          List(
            ParameterS(AtomSP(Some(CaptureP("this", FinalP)), Some(AbstractSP), "T", None))),
          Some("V"),
          List(
            EqualsAR(
              TemplexAR(RuneAT("T", CoordTemplataType)),
              ComponentsAR(
                CoordTemplataType,
                List(
                  OrAR(List(TemplexAR(OwnershipAT(OwnP)), TemplexAR(OwnershipAT(ShareP)))),
                  CallAR(
                    "passThroughIfInterface",
                    List(TemplexAR(AnonymousRuneAT(KindTemplataType))),
                    KindTemplataType)))),
            EqualsAR(
              TemplexAR(RuneAT("V", CoordTemplataType)),
              TemplexAR(NameAT("Void", CoordTemplataType)))),
          GeneratedBodyA("interfaceDestructorGenerator"))),
      functionGeneratorByName +
        ("interfaceDestructorGenerator" ->
        new IFunctionGenerator {
          override def generate(
            innerEnv: FunctionEnvironmentBox,
            temputs: TemputsBox,
            maybeOriginFunction1: Option[FunctionA],
            params: List[Parameter2],
            maybeReturnType2: Option[Coord]):
          (FunctionHeader2) = {
            // Even though below we treat packs, closures, and structs the same, they're
            // still disambiguated by the template arguments.
            val Some(returnType2) = maybeReturnType2
            params.map(_.tyype) match {
              case List(Coord(_, InterfaceRef2(_))) => {
                FunctionTemplarCore.makeInterfaceFunction(
                  innerEnv,
                  temputs,
                  maybeOriginFunction1,
                  params,
                  returnType2)
              }
              case _ => {
                vfail("wot")
              }
            }
          }
        }))
  }

  private def addImplDestructor(
    currentlyConstructingEnv: NamespaceEnvironment,
    functionGeneratorByName: Map[String, IFunctionGenerator]
  ): (NamespaceEnvironment, Map[String, IFunctionGenerator]) = {
    (
    currentlyConstructingEnv
      .addFunction(
        FunctionA(
          CodeLocation(CallTemplar.INTERFACE_DESTRUCTOR_NAME + ".builtin.vale", 0, 1),
          CallTemplar.INTERFACE_DESTRUCTOR_NAME,
          List(),
          0,
          true,
          TemplateTemplataType(List(CoordTemplataType, KindTemplataType), FunctionTemplataType),
          List("T", "I"),
          Map("T" -> CoordTemplataType, "I" -> KindTemplataType),
          List(
            ParameterS(AtomSP(Some(CaptureP("this", FinalP)), Some(OverrideSP("I")), "T", None))),
          Some("V"),
          List(
            EqualsAR(
              TemplexAR(RuneAT("T", CoordTemplataType)),
              ComponentsAR(
                CoordTemplataType,
                List(
                  OrAR(List(TemplexAR(OwnershipAT(OwnP)), TemplexAR(OwnershipAT(ShareP)))),
                  CallAR(
                    "passThroughIfStruct",
                    List(TemplexAR(AnonymousRuneAT(KindTemplataType))),
                    KindTemplataType)))),
            CallAR("passThroughIfInterface", List(TemplexAR(RuneAT("I", KindTemplataType))), KindTemplataType),
            EqualsAR(
              TemplexAR(RuneAT("V", CoordTemplataType)),
              TemplexAR(NameAT("Void", CoordTemplataType)))),
          GeneratedBodyA("implDestructorGenerator"))),
      functionGeneratorByName + (
        "implDestructorGenerator" ->
          new IFunctionGenerator {
            override def generate(
              innerEnv: FunctionEnvironmentBox,
              temputs: TemputsBox,
              maybeOriginFunction1: Option[FunctionA],
              params: List[Parameter2],
              maybeReturnType2: Option[Coord]):
            (FunctionHeader2) = {
              // There are multiple idestructor overrides for a given struct, which can
              // confuse us.
              // They all override different interfaces, but that's not factored into the
              // overload templar.
              // However, the template arguments are, and idestructor's template argument
              // is the interface we're overriding.
              val List(
              CoordTemplata(Coord(_, overridingStructRef2FromTemplateArg @ StructRef2(_))),
              KindTemplata(implementedInterfaceRef2 @ InterfaceRef2(_))) =
              innerEnv.fullName.steps.last.templateArgs.get

              params.map(_.tyype) match {
                case List(Coord(_, structRef2 @ StructRef2(_))) => {
                  vassert(overridingStructRef2FromTemplateArg == structRef2)
                  val structDef2 = temputs.lookupStruct(structRef2)
                  FunctionTemplarCore.makeImplDestructor(
                    innerEnv.globalEnv, temputs, maybeOriginFunction1, structDef2, implementedInterfaceRef2)
                }
                case _ => {
                  vfail("wot")
                }
              }
            }
          }))
  }

  private def addDrop(
    currentlyConstructingEnv: NamespaceEnvironment,
    functionGeneratorByName: Map[String, IFunctionGenerator]
  ): (NamespaceEnvironment, Map[String, IFunctionGenerator]) = {
    // Drop is a function that:
    // - If received an owning pointer, will call the destructor
    // - If received a share pointer, will decrement it and if was last, call its destructor
    // - If received a borrow, do nothing.
    // Conceptually it's "drop the reference", as opposed to destructor which is "drop the object"
    (
      currentlyConstructingEnv
        .addFunction(
          FunctionA(
            CodeLocation(CallTemplar.DROP_FUNCTION_NAME + ".builtin.vale", 0, 0),
            CallTemplar.DROP_FUNCTION_NAME,
            List(),
            0,
            true,
            TemplateTemplataType(List(CoordTemplataType), FunctionTemplataType),
            List("T"),
            Map("T" -> CoordTemplataType),
            List(
              ParameterS(AtomSP(Some(CaptureP("x", FinalP)), None, "T", None))),
            Some("V"),
            List(
              TemplexAR(RuneAT("T", CoordTemplataType)),
              EqualsAR(
                TemplexAR(RuneAT("V", CoordTemplataType)),
                TemplexAR(NameAT("Void", CoordTemplataType)))),
            GeneratedBodyA("dropGenerator"))),
      functionGeneratorByName + (
        "dropGenerator" ->
          new IFunctionGenerator {
            override def generate(
              innerEnv: FunctionEnvironmentBox,
              temputs: TemputsBox,
              maybeOriginFunction1: Option[FunctionA],
              params: List[Parameter2],
              maybeReturnType2: Option[Coord]):
            (FunctionHeader2) = {
              vassert(maybeReturnType2 == Some(Coord(Raw, Void2())))
              val List(CoordTemplata(ref2)) = innerEnv.fullName.steps.last.templateArgs.get
              val List(Parameter2("x", None, paramType2)) = params
              vassert(paramType2 == ref2)
              DestructorTemplar.generateDropFunction(
                innerEnv, temputs, maybeOriginFunction1.get, ref2)
            }
          }))
  }

  private def addArrayLen(currentlyConstructingEnv: NamespaceEnvironment): NamespaceEnvironment = {
    currentlyConstructingEnv
      .addFunction(
        FunctionA(
          CodeLocation("len.builtin.vale", 0, 0),
          "len",
          List(),
          0,
          true,
          TemplateTemplataType(List(CoordTemplataType), FunctionTemplataType),
          List("T"),
          Map("T" -> CoordTemplataType),
          List(
            ParameterS(AtomSP(Some(CaptureP("arr", FinalP)), None, "T", None))),
          Some("I"),
          List(
            EqualsAR(
              TemplexAR(RuneAT("T", CoordTemplataType)),
              ComponentsAR(
                CoordTemplataType,
                List(
                  OrAR(List(TemplexAR(OwnershipAT(BorrowP)), TemplexAR(OwnershipAT(ShareP)))),
                  TemplexAR(
                    CallAT(
                      NameAT("__Array", TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), KindTemplataType)),
                      List(
                        AnonymousRuneAT(MutabilityTemplataType),
                        AnonymousRuneAT(CoordTemplataType)),
                      KindTemplataType))))),
            EqualsAR(
              TemplexAR(RuneAT("I", CoordTemplataType)),
              TemplexAR(NameAT("Int", CoordTemplataType)))),
          CodeBodyA(
            BodyAE(
              Set(),
              BlockAE(
                Set(LocalVariable1("arr", FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
                List(
                  ArrayLengthAE(
                    LocalLoadAE("arr", false))))))))
      .addFunction(
        FunctionA(
          CodeLocation("len.builtin.vale", 0, 1),
          "len",
          List(),
          0,
          true,
          TemplateTemplataType(List(CoordTemplataType), FunctionTemplataType),
          List("N", "T"),
          Map(
            "A" -> CoordTemplataType,
            "N" -> IntegerTemplataType,
            "T" -> CoordTemplataType),
          List(
            ParameterS(AtomSP(Some(CaptureP("arr", FinalP)), None, "A", None))),
          Some("I"),
          List(
            EqualsAR(
              TemplexAR(RuneAT("A", CoordTemplataType)),
              ComponentsAR(
                CoordTemplataType,
                List(
                  TemplexAR(OwnershipAT(BorrowP)),
                  TemplexAR(
                    RepeaterSequenceAT(
                      RuneAT("M", MutabilityTemplataType),
                      RuneAT("N", IntegerTemplataType),
                      RuneAT("T", CoordTemplataType),
                      KindTemplataType))))),
            EqualsAR(
              TemplexAR(RuneAT("I", CoordTemplataType)),
              TemplexAR(NameAT("Int", CoordTemplataType)))),
          CodeBodyA(
            BodyAE(
              Set(),
              BlockAE(
                Set(LocalVariable1("arr", FinalP, NotUsed, Used, NotUsed, NotUsed, NotUsed, NotUsed)),
                List(
                  LocalLoadAE("N", false)))))))
  }


  private def addPanic(currentlyConstructingEnv: NamespaceEnvironment): NamespaceEnvironment = {
    currentlyConstructingEnv
      .addFunction(
        FunctionA(
          CodeLocation("panic.builtin.vale", 0, 0),
          "panic",
          List(),
          0,
          true,
          FunctionTemplataType,
          List(),
          Map(),
          List(),
          Some("N"),
          List(
            EqualsAR(
              TemplexAR(RuneAT("N", CoordTemplataType)),
              TemplexAR(NameAT("__Never", CoordTemplataType)))),
          ExternBodyA))
  }
}
