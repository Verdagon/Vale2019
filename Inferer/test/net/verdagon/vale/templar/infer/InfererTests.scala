package net.verdagon.vale.templar.infer

import net.verdagon.vale.astronomer._
import net.verdagon.vale.astronomer.ruletyper.IRuleTyperEvaluatorDelegate
import net.verdagon.vale.parser._
import net.verdagon.vale.scout._
import net.verdagon.vale.scout.patterns.{AbstractSP, AtomSP}
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.templata._
import org.scalamock.scalatest.MockFactory
import net.verdagon.vale.templar.types._
import net.verdagon.vale.{vassert, vassertSome, vfail}
import org.scalatest.{FunSuite, Matchers}
import net.verdagon.vale.templar.infer.inferer._

import scala.collection.immutable.List

case class FakeEnv()
case class FakeState()

object InfererTestUtils {
  def getMutability(kind: Kind): Mutability = {
    kind match {
      case Int2() => Immutable
      case StructRef2(fullName) if fullName.steps.last.humanName.startsWith("Imm") => Immutable
      case StructRef2(fullName) if fullName.steps.last.humanName.startsWith("Mut") => Mutable
      case InterfaceRef2(fullName) if fullName.steps.last.humanName.startsWith("Imm") => Immutable
      case InterfaceRef2(fullName) if fullName.steps.last.humanName.startsWith("Mut") => Mutable
      case ArraySequenceT2(_, RawArrayT2(_, mutability)) => mutability
      case UnknownSizeArrayT2(RawArrayT2(_, mutability)) => mutability
    }
  }
}

case class SimpleEnvironment(entries: Map[String, IEnvEntry]) extends IEnvironment {
  def fullName = FullName2(List())
  def globalEnv: NamespaceEnvironment = {
    vfail()
  }
  override def getAllTemplatasWithName(name: String, lookupFilter: Set[ILookupContext]): List[ITemplata] = {
    entries.get(name).toList.map(EnvironmentUtils.entryToTemplata(this, _))
  }
  override def getNearestTemplataWithName(name: String, lookupFilter: Set[ILookupContext]): Option[ITemplata] = {
    entries.get(name).map(EnvironmentUtils.entryToTemplata(this, _))
  }
}

class FakeInfererEvaluatorDelegate extends IInfererEvaluatorDelegate[SimpleEnvironment, FakeState] {
  override def getAncestorInterfaceDistance(temputs: FakeState, descendantCitizenRef: CitizenRef2, ancestorInterfaceRef: InterfaceRef2): (Option[Int]) = {
    vfail()
  }

  override def getAncestorInterfaces(temputs: FakeState, descendantCitizenRef: CitizenRef2): (Set[InterfaceRef2]) = {
    vfail()
  }

  override def getMutability(state: FakeState, kind: Kind): Mutability = {
    InfererTestUtils.getMutability(kind)
  }

  override def lookupMemberTypes(state: FakeState, kind: Kind, expectedNumMembers: Int): Option[List[Coord]] = {
    vfail()
  }

  override def getMemberCoords(state: FakeState, structRef: StructRef2): List[Coord] = {
    vfail()
  }

  override def citizenIsFromTemplate(state: FakeState, citizen: CitizenRef2, template: ITemplata): (Boolean) = {
    vfail()
  }
}

class FakeTemplataTemplarInnerDelegate extends ITemplataTemplarInnerDelegate[SimpleEnvironment, FakeState] {
  override def evaluateInterfaceTemplata(state: FakeState, templata: InterfaceTemplata, templateArgs: List[ITemplata]): (Kind) = {
    vfail()
  }
  override def evaluateStructTemplata(state: FakeState, templata: StructTemplata, templateArgs: List[ITemplata]): (Kind) = {
    vfail()
  }
  override def getAncestorInterfaceDistance(state: FakeState, descendantCitizenRef: CitizenRef2, ancestorInterfaceRef: InterfaceRef2): (Option[Int]) = {
    vfail()
  }
  override def getMutability(state: FakeState, kind: Kind): Mutability = {
    InfererTestUtils.getMutability(kind)
  }
  override def getPackKind(env: SimpleEnvironment, state: FakeState, types2: List[Coord]): (PackT2, Mutability) = {
    vfail()
  }
  override def lookupTemplata(env: SimpleEnvironment, name: String): ITemplata = {
    vassertSome(env.getNearestTemplataWithName(name, Set(TemplataLookupContext)))
  }

  override def getArraySequenceKind(env: SimpleEnvironment, state: FakeState, mutability: Mutability, size: Int, element: Coord): (ArraySequenceT2) = {
    vfail()
  }

  override def getInterfaceTemplataType(it: InterfaceTemplata): TemplateTemplataType = {
    vfail()
  }

  override def getStructTemplataType(st: StructTemplata): TemplateTemplataType = {
    vfail()
  }
}

class InfererTests extends FunSuite with Matchers with MockFactory {
  def makeCannedEnvironment(): SimpleEnvironment = {
    SimpleEnvironment(
      Map(
        "ImmInterface" -> InterfaceEnvEntry(InterfaceA(CodeLocation("ImmInterface.vale", 0, 0), List(), "ImmInterface", ImmutableP, Some(ImmutableP), KindTemplataType, List(), Map(), List())),
        "Array" -> TemplataEnvEntry(ArrayTemplateTemplata()),
        "MutTStruct" -> StructEnvEntry(StructA(CodeLocation("MutTStruct.vale", 0, 0), List(), "MutTStruct", MutableP, Some(MutableP), TemplateTemplataType(List(CoordTemplataType), KindTemplataType), List("T"), Map("T" -> CoordTemplataType), List(), List())),
        "MutTInterface" -> InterfaceEnvEntry(InterfaceA(CodeLocation("MutTInterface.vale", 0, 0), List(), "MutTInterface", MutableP, Some(MutableP), TemplateTemplataType(List(CoordTemplataType), KindTemplataType), List("T"), Map("T" -> CoordTemplataType), List())),
        "MutStruct" -> StructEnvEntry(StructA(CodeLocation("MutStruct.vale", 0, 0), List(), "MutStruct", MutableP, Some(MutableP), KindTemplataType, List(), Map(), List(), List())),
        "MutInterface" -> InterfaceEnvEntry(InterfaceA(CodeLocation("MutInterface.vale", 0, 0), List(), "MutInterface", MutableP, Some(MutableP), KindTemplataType, List(), Map(), List())),
        "MutStructBorrow" -> TemplataEnvEntry(CoordTemplata(Coord(Borrow, StructRef2(FullName2(List(NamePart2("MutStruct", Some(List())))))))),
        "MutArraySequenceOf4Int" -> TemplataEnvEntry(KindTemplata(ArraySequenceT2(4, RawArrayT2(Coord(Share, Int2()), Mutable)))),
        "Void" -> TemplataEnvEntry(KindTemplata(Void2())),
        "Int" -> TemplataEnvEntry(KindTemplata(Int2()))))
  }

  // Makes an evaluator with some canned data
  def makeCannedEvaluator(): InfererEvaluator[SimpleEnvironment, FakeState] = {
    val templataTemplarDelegate =
      new FakeTemplataTemplarInnerDelegate() {
        override def getMutability(state: FakeState, kind: Kind): Mutability = {
          kind match {
            case StructRef2(fullName) if fullName.steps.last.humanName.startsWith("Mut") => Mutable
            case StructRef2(fullName) if fullName.steps.last.humanName.startsWith("Imm") => Immutable
            case InterfaceRef2(fullName) if fullName.steps.last.humanName.startsWith("Mut") => Mutable
            case InterfaceRef2(fullName) if fullName.steps.last.humanName.startsWith("Imm") => Immutable
            case Int2() => Immutable
            case ArraySequenceT2(_, RawArrayT2(_, mutability)) => mutability
            case UnknownSizeArrayT2(RawArrayT2(_, mutability)) => mutability
            case Void2() => Immutable
            case _ => vfail()
          }
        }
        override def evaluateInterfaceTemplata(state: FakeState, templata: InterfaceTemplata, templateArgs: List[ITemplata]): (Kind) = {
          (templata, templateArgs) match {
            case (InterfaceTemplata(_,interfaceName("MutTInterface")), List(CoordTemplata(Coord(Share, Int2())) )) => {
              (InterfaceRef2(FullName2(List(NamePart2("MutTInterface", Some(List(CoordTemplata(Coord(Share, Int2())))))))))
            }
            case (InterfaceTemplata(_,interfaceName("MutInterface")), List()) => {
              (InterfaceRef2(FullName2(List(NamePart2("MutInterface", Some(List()))))))
            }
            case (InterfaceTemplata(_,interfaceName("ImmInterface")), List()) => {
              (InterfaceRef2(FullName2(List(NamePart2("ImmInterface", Some(List()))))))
            }
          }
        }

        override def evaluateStructTemplata(state: FakeState, templata: StructTemplata, templateArgs: List[ITemplata]): (Kind) = {
          (templata, templateArgs) match {
            case (StructTemplata(_,structName("MutTStruct")), List(CoordTemplata(Coord(Share, Int2())) )) => {
              (StructRef2(FullName2(List(NamePart2("MutTStruct", Some(List(CoordTemplata(Coord(Share, Int2())))))))))
            }
            case (StructTemplata(_,structName("MutStruct")), List()) => {
              (StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()))))))
            }
          }
        }
        override def getInterfaceTemplataType(it: InterfaceTemplata): TemplateTemplataType = {
          it match {
            case InterfaceTemplata(_,interfaceName("MutTInterface")) => {
              TemplateTemplataType(List(CoordTemplataType), KindTemplataType)
            }
            case InterfaceTemplata(_, interfaceName("MutInterface")) => vfail()
          }
        }
        override def getStructTemplataType(it: StructTemplata): TemplateTemplataType = {
          it match {
            case StructTemplata(_,structName("MutTStruct")) => TemplateTemplataType(List(CoordTemplataType), KindTemplataType)
          }
        }
        override def getArraySequenceKind(env: SimpleEnvironment, state: FakeState, mutability: Mutability, size: Int, element: Coord): (ArraySequenceT2) = {
          (ArraySequenceT2(size, RawArrayT2(element, mutability)))
        }
      }
    val delegate =
      new FakeInfererEvaluatorDelegate() {
        override def getAncestorInterfaces(state: FakeState, descendantCitizenRef: CitizenRef2): (Set[InterfaceRef2]) = {
          descendantCitizenRef match {
            case StructRef2(FullName2(List(NamePart2("MutTStruct",Some(List(CoordTemplata(Coord(Share, Int2())))))))) => (Set(InterfaceRef2(FullName2(List(NamePart2("MutTInterface", Some(List(CoordTemplata(Coord(Share, Int2()))))))))))
            case StructRef2(FullName2(List(NamePart2("MutStruct",Some(List()))))) => (Set(InterfaceRef2(FullName2(List(NamePart2("MutInterface", Some(List())))))))
            case InterfaceRef2(FullName2(List(NamePart2("MutInterface",Some(List()))))) => (Set())
            case StructRef2(FullName2(List(NamePart2("MutSoloStruct",Some(List()))))) => (Set())
            case _ => vfail(descendantCitizenRef.toString)
          }
        }

        override def citizenIsFromTemplate(state: FakeState, citizen: CitizenRef2, template: ITemplata): (Boolean) = {
          (citizen, template) match {
            case (InterfaceRef2(FullName2(List(NamePart2("MutTInterface",Some(List(CoordTemplata(Coord(Share,Int2())))))))), InterfaceTemplata(_, interfaceName("MutTInterface"))) => (true)
            case (StructRef2(FullName2(List(NamePart2("MutTStruct",Some(List(CoordTemplata(Coord(Share,Int2())))))))), StructTemplata(_, structName("MutTStruct"))) => (true)
            case (StructRef2(FullName2(List(NamePart2("MutTStruct",Some(List(CoordTemplata(Coord(Share,Int2())))))))), InterfaceTemplata(_, interfaceName("MutTInterface"))) => (false)
            case _ => vfail()
          }
        }
      }

    makeEvaluator(Some(templataTemplarDelegate), Some(delegate))
  }

  def makeEvaluator(
    maybeTemplataTemplarDelegate: Option[FakeTemplataTemplarInnerDelegate],
    maybeEvaluatorDelegate: Option[FakeInfererEvaluatorDelegate]):
  InfererEvaluator[SimpleEnvironment, FakeState] = {
    val templataTemplar =
      new TemplataTemplarInner[SimpleEnvironment, FakeState](
        maybeTemplataTemplarDelegate match {
          case None => new FakeTemplataTemplarInnerDelegate()
          case Some(t) => t
        })

    val equalsLayer =
      new InfererEquator[SimpleEnvironment, FakeState](
        templataTemplar)
    val inferEvaluatorDelegate =
      maybeEvaluatorDelegate match {
        case Some(e) => e
        case None => new FakeInfererEvaluatorDelegate()
      }
    val evaluator =
      new InfererEvaluator[SimpleEnvironment, FakeState](
        templataTemplar,
        equalsLayer,
        inferEvaluatorDelegate)
    evaluator
  }

  test("Borrow becomes share if kind is immutable") {
    val (InferSolveSuccess(inferences)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            TemplexAR(RuneAT("__C", CoordTemplataType)),
            EqualsAR(
              TemplexAR(RuneAT("__C", CoordTemplataType)),
              TemplexAR(OwnershippedAT(BorrowP,NameAT("ImmInterface", CoordTemplataType))))),
          Map("__C" -> CoordTemplataType),
          Map(),
          List(),
          None,
          true)

    vassert(inferences.templatasByRune("__C") == CoordTemplata(Coord(Share, InterfaceRef2(FullName2(List(NamePart2("ImmInterface", Some(List()))))))))
  }

  test("Can infer coord rune from an incoming kind") {
    val (isf @ InferSolveFailure(_, _, _,_,_, _)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(TemplexAR(RuneAT("C", CoordTemplataType))),
          Map("C" -> CoordTemplataType),
          Map("C" -> KindTemplata(InterfaceRef2(FullName2(List(NamePart2("ImmInterface",Some(List(KindTemplata(Int2()))))))))),
          List(),
          None,
          true)

    vassert(isf.toString.contains("doesn't match expected type"))
  }

  test("Detects conflict between types") {
    val (isf @ InferSolveFailure(_, _, _,_,_, _)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(EqualsAR(TemplexAR(RuneAT("C", CoordTemplataType)), TemplexAR(RuneAT("A", KindTemplataType)))),
          Map("A" -> KindTemplataType),
          Map("A" -> KindTemplata(Int2())),
          List(),
          None,
          true)

    vassert(isf.toString.contains("Doesn't match type!"))
  }

  test("Can explicitly coerce from kind to coord") {
    val (InferSolveSuccess(conclusions)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            EqualsAR(
              TemplexAR(RuneAT("C", CoordTemplataType)),
              CallAR("toRef", List(TemplexAR(RuneAT("A", KindTemplataType))), CoordTemplataType))),
          Map("C" -> CoordTemplataType, "A" -> KindTemplataType),
          Map("A" -> KindTemplata(Int2())),
          List(),
          None,
          true)

    conclusions.templatasByRune("C") shouldEqual CoordTemplata(Coord(Share, Int2()))
  }

  test("Can explicitly coerce from kind to coord 2") {
    val (InferSolveSuccess(conclusions)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            TemplexAR(RuneAT("__ParamRune_0", CoordTemplataType)),
            EqualsAR(TemplexAR(RuneAT("__ParamRune_0", CoordTemplataType)),TemplexAR(NameAT("Int", CoordTemplataType)))),
          Map("__ParamRune_0" -> CoordTemplataType),
          Map(),
          List(),
          None,
          true)

    conclusions.templatasByRune("__ParamRune_0") shouldEqual CoordTemplata(Coord(Share, Int2()))
  }

  test("Can match KindTemplataType against StructEnvEntry / StructTemplata") {
    val (InferSolveSuccess(conclusions)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            EqualsAR(
              TemplexAR(RuneAT("__RetRune", CoordTemplataType)),
              CallAR("toRef",List(TemplexAR(NameAT("MutStruct", KindTemplataType))), TemplateTemplataType(List(KindTemplataType), CoordTemplataType)))),
          Map("__RetRune" -> CoordTemplataType),
          Map(),
          List(),
          None,
          true)

    conclusions.templatasByRune("__RetRune") shouldEqual CoordTemplata(Coord(Own, StructRef2(FullName2(List(NamePart2("MutStruct", Some(List())))))))
  }

  test("Can infer from simple rules") {
    val (InferSolveSuccess(inferences)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            TemplexAR(RuneAT("__ParamRune_0", CoordTemplataType)),
            EqualsAR(TemplexAR(RuneAT("__ParamRune_0", CoordTemplataType)),CallAR("toRef", List(TemplexAR(NameAT("Int", KindTemplataType))), CoordTemplataType))),
          Map("__ParamRune_0" -> CoordTemplataType),
          Map(),
          List(),
          None,
          true)

    vassert(inferences.templatasByRune("__ParamRune_0") == CoordTemplata(Coord(Share, Int2())))
  }

  test("Can infer templata from CallAT") {
    val (InferSolveSuccess(inferences)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            EqualsAR(
              TemplexAR(RuneAT("X", KindTemplataType)),
              TemplexAR(CallAT(NameAT("MutTInterface", TemplateTemplataType(List(CoordTemplataType), KindTemplataType)),List(RuneAT("T", CoordTemplataType)), KindTemplataType)))),
          Map("X" -> KindTemplataType, "T" -> CoordTemplataType),
          Map("X" -> KindTemplata(InterfaceRef2(FullName2(List(NamePart2("MutTInterface",Some(List(CoordTemplata(Coord(Share, Int2())))))))))),
          List(),
          None,
          true)

    vassert(inferences.templatasByRune("T") == CoordTemplata(Coord(Share, Int2())))
  }

  test("Can conjure an owning coord from a borrow coord") {
    val (InferSolveSuccess(inferences)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            TemplexAR(RuneAT("T", CoordTemplataType)),
            TemplexAR(RuneAT("__ParamRune_0K", KindTemplataType)),
            EqualsAR(
              TemplexAR(RuneAT("T", CoordTemplataType)),
              ComponentsAR(
                CoordTemplataType,
                List(TemplexAR(OwnershipAT(OwnP)), TemplexAR(RuneAT("__ParamRune_0K", KindTemplataType))))),
            TemplexAR(RuneAT("__ParamRune_0", CoordTemplataType)),
            EqualsAR(
              TemplexAR(RuneAT("__ParamRune_0", CoordTemplataType)),
              ComponentsAR(
                CoordTemplataType,
                List(TemplexAR(OwnershipAT(BorrowP)), TemplexAR(RuneAT("__ParamRune_0K", KindTemplataType)))))),
          Map("__ParamRune_0K" -> KindTemplataType, "__ParamRune_0" -> CoordTemplataType),
          Map(),
          List(AtomSP(Some(CaptureP("m",FinalP)),None,"__ParamRune_0",None)),
          Some(List(ParamFilter(Coord(Borrow,InterfaceRef2(FullName2(List(NamePart2("MutInterface", Some(List())))))),None))),
          true)

    vassert(inferences.templatasByRune("T") == CoordTemplata(Coord(Own,InterfaceRef2(FullName2(List(NamePart2("MutInterface", Some(List()))))))))
  }

  test("Rune 0 upcasts to right type, simple") {
    val (InferSolveSuccess(inferences)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            TemplexAR(RuneAT("__Let0_", CoordTemplataType)),
            EqualsAR(
              TemplexAR(RuneAT("__Let0_", CoordTemplataType)),
              CallAR("toRef", List(TemplexAR(NameAT("MutInterface", KindTemplataType))), CoordTemplataType))),
          Map("__Let0_" -> KindTemplataType),
          Map(),
          List(AtomSP(Some(CaptureP("x",FinalP)),None,"__Let0_",None)),
          Some(List(ParamFilter(Coord(Own,StructRef2(FullName2(List(NamePart2("MutStruct",Some(List())))))),None))),
          true)

    vassert(inferences.templatasByRune("__Let0_") == CoordTemplata(Coord(Own, InterfaceRef2(FullName2(List(NamePart2("MutInterface", Some(List()))))))))
  }

  test("Rune 0 upcasts to right type templated") {
    val (InferSolveSuccess(inferences)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            TemplexAR(RuneAT("__Let0_", CoordTemplataType)),
            EqualsAR(
              TemplexAR(RuneAT("__Let0_", CoordTemplataType)),
              CallAR(
                "toRef",
                List(
                  TemplexAR(
                    CallAT(
                      NameAT("MutTInterface", TemplateTemplataType(List(CoordTemplataType), KindTemplataType)),
                      List(RuneAT("T", CoordTemplataType)),
                      KindTemplataType))),
                CoordTemplataType))),
          Map("__Let0_" -> KindTemplataType, "T" -> CoordTemplataType),
          Map(),
          List(AtomSP(Some(CaptureP("x",FinalP)),None,"__Let0_",None)),
          Some(List(ParamFilter(Coord(Own,StructRef2(FullName2(List(NamePart2("MutTStruct",Some(List(CoordTemplata(Coord(Share, Int2()))))))))),None))),
          true)

    vassert(inferences.templatasByRune("__Let0_") == CoordTemplata(Coord(Own, InterfaceRef2(FullName2(List(NamePart2("MutTInterface", Some(List(CoordTemplata(Coord(Share, Int2())))))))))))
    vassert(inferences.templatasByRune("T") == CoordTemplata(Coord(Share, Int2())))
  }

  test("Tests destructor") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        EqualsAR(
          TemplexAR(RuneAT("T", CoordTemplataType)),
          ComponentsAR(
            CoordTemplataType,
            List(
              OrAR(List(TemplexAR(OwnershipAT(OwnP)), TemplexAR(OwnershipAT(ShareP)))),
              CallAR("passThroughIfConcrete",List(TemplexAR(AnonymousRuneAT(KindTemplataType))), KindTemplataType)))),
        EqualsAR(TemplexAR(RuneAT("V", CoordTemplataType)),CallAR("toRef",List(TemplexAR(NameAT("Void",KindTemplataType))), CoordTemplataType)))
    val atoms =
      List(AtomSP(Some(CaptureP("this",FinalP)),None,"T",None))

    val solve =
      (paramFilter: ParamFilter) => {
        makeCannedEvaluator().solve(
          makeCannedEnvironment(),
          FakeState(),
          rules,
          Map("V" -> CoordTemplataType, "T" -> CoordTemplataType),
          Map(),
          atoms,
          Some(List(paramFilter)),
          true)
      }

    // Test that it does match a pack
    val packCoord = Coord(Share,PackT2(List(),StructRef2(FullName2(List(NamePart2("__Pack",Some(List())))))))
    val (InferSolveSuccess(inferencesA)) = solve(ParamFilter(packCoord,None))
    vassert(inferencesA.templatasByRune("T") == CoordTemplata(packCoord))

    // Test that it does match a struct
    val structCoord = Coord(Own,StructRef2(FullName2(List(NamePart2("MutStruct",Some(List()))))))
    val (InferSolveSuccess(inferencesD)) = solve(ParamFilter(structCoord,None))
    vassert(inferencesD.templatasByRune("T") == CoordTemplata(structCoord))

    // Test that it doesn't match an int
    val intCoord = Coord(Share,Int2())
    val (isfE @ InferSolveFailure(_, _,_,_, _, _)) = solve(ParamFilter(intCoord,None))
    vassert(isfE.toString.contains("Bad arguments to passThroughIfConcrete"))

    // Test that it doesn't match an interface
    val interfaceCoord = Coord(Own,InterfaceRef2(FullName2(List(NamePart2("MutInterface",Some(List()))))))
    val (isfF @ InferSolveFailure(_, _, _,_,_, _)) = solve(ParamFilter(interfaceCoord,None))
    vassert(isfF.toString.contains("Bad arguments to passThroughIfConcrete"))
  }

  test("Tests passThroughIfInterface") {
    // Tests that we can make a rule that will only match interfaces.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        EqualsAR(
          TemplexAR(RuneAT("T", CoordTemplataType)),
          ComponentsAR(
            CoordTemplataType,
            List(
              OrAR(List(TemplexAR(OwnershipAT(OwnP)), TemplexAR(OwnershipAT(ShareP)))),
              CallAR("passThroughIfInterface",List(TemplexAR(AnonymousRuneAT(KindTemplataType))), KindTemplataType)))),
        EqualsAR(
          TemplexAR(RuneAT("V", CoordTemplataType)),
          CallAR("toRef",List(TemplexAR(NameAT("Void", KindTemplataType))), CoordTemplataType)))
    val atoms =
      List(AtomSP(Some(CaptureP("this",FinalP)),None,"T",None))

    val solve =
      (paramFilter: ParamFilter) => {
        makeCannedEvaluator().solve(
          makeCannedEnvironment(),
          FakeState(),
          rules,
          Map("T" -> CoordTemplataType, "V" -> CoordTemplataType),
          Map(),
          atoms,
          Some(List(paramFilter)),
          true)
      }

    // Test that it does match an interface
    val interfaceCoord = Coord(Own,InterfaceRef2(FullName2(List(NamePart2("MutInterface",Some(List()))))))
    val (InferSolveSuccess(inferencesD)) = solve(ParamFilter(interfaceCoord,None))
    vassert(inferencesD.templatasByRune("T") == CoordTemplata(interfaceCoord))

    // Test that it doesn't match an int
    val intCoord = Coord(Share,Int2())
    val (isfE @ InferSolveFailure(_, _, _, _,_,_)) = solve(ParamFilter(intCoord,None))
    vassert(isfE.toString.contains("Bad arguments to passThroughIfInterface"))

    // TODO: make a more accurate test that tests a struct doesn't match. Tried doing
    // it like the int, but since its handed in as a parameter, it just upcasted! LOL
  }


  test("Tests passThroughIfStruct") {
    // Tests that we can make a rule that will only match structs.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        EqualsAR(
          TemplexAR(RuneAT("T", CoordTemplataType)),
          ComponentsAR(
            CoordTemplataType,
            List(
              OrAR(List(TemplexAR(OwnershipAT(OwnP)), TemplexAR(OwnershipAT(ShareP)))),
              CallAR("passThroughIfStruct",List(TemplexAR(AnonymousRuneAT(KindTemplataType))), KindTemplataType)))))
    val atoms =
      List(AtomSP(Some(CaptureP("this",FinalP)),None,"T",None))

    val solve =
      (paramFilter: ParamFilter) => {
        makeCannedEvaluator().solve(
          makeCannedEnvironment(),
          FakeState(),
          rules,
          Map("T" -> CoordTemplataType),
          Map(),
          atoms,
          Some(List(paramFilter)),
          true)
      }

    // Test that it does match a struct
    val structCoord = Coord(Own,StructRef2(FullName2(List(NamePart2("MutStruct",Some(List()))))))
    val (InferSolveSuccess(inferencesD)) = solve(ParamFilter(structCoord,None))
    vassert(inferencesD.templatasByRune("T") == CoordTemplata(structCoord))

    // Test that it doesn't match an int
    val intCoord = Coord(Share,Int2())
    val (isfE @ InferSolveFailure(_, _, _,_,_, _)) = solve(ParamFilter(intCoord,None))
    vassert(isfE.toString.contains("Bad arguments to passThroughIfStruct"))

    // Test that it doesn't match an interface
    val interfaceCoord = Coord(Own,InterfaceRef2(FullName2(List(NamePart2("MutInterface",Some(List()))))))
    val (isfF @ InferSolveFailure(_, _, _,_,_, _)) = solve(ParamFilter(interfaceCoord,None))
    vassert(isfF.toString.contains("Bad arguments to passThroughIfStruct"))

    // Test that it doesn't match an pack
    val packCoord = Coord(Share,PackT2(List(),StructRef2(FullName2(List(NamePart2("__Pack",Some(List())))))))
    val (isfG @ InferSolveFailure(_, _, _,_,_, _)) = solve(ParamFilter(packCoord,None))
    vassert(isfG.toString.contains("Bad arguments to passThroughIfStruct"))
  }

  test("Test coercing template call result") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        TemplexAR(RuneAT("__ParamRune_0", CoordTemplataType)),
        EqualsAR(
          TemplexAR(RuneAT("__ParamRune_0", CoordTemplataType)),
          TemplexAR(
            CallAT(
              NameAT("MutTStruct", TemplateTemplataType(List(CoordTemplataType), KindTemplataType)),
              List(NameAT("Int", CoordTemplataType)),
              CoordTemplataType))))
    val atoms =
      List(AtomSP(Some(CaptureP("this",FinalP)),None,"T",None))

    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        rules,
        Map("__ParamRune_0" -> CoordTemplataType),
        Map(),
        atoms,
        None,
        true)

    inferencesD.templatasByRune("__ParamRune_0") shouldEqual
      CoordTemplata(Coord(Own,StructRef2(FullName2(List(NamePart2("MutTStruct",Some(List(CoordTemplata(Coord(Share,Int2()))))))))))
  }


  test("Test result of a CallAT can coerce to coord") {
    val rules =
      List(
        TemplexAR(RuneAT("__Par0", CoordTemplataType)),
        EqualsAR(TemplexAR(RuneAT("__Par0", CoordTemplataType)),TemplexAR(NameAT("MutStruct", CoordTemplataType))))
    val atoms =
      List(AtomSP(Some(CaptureP("this",FinalP)),None,"T",None))

    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        rules,
        Map("__Par0" -> CoordTemplataType),
        Map(),
        atoms,
        None,
        true)
    inferencesD.templatasByRune("__Par0") shouldEqual
      CoordTemplata(Coord(Own,StructRef2(FullName2(List(NamePart2("MutStruct",Some(List())))))))
  }

  test("Matching a CoordTemplataType onto a CallAT") {
    val rules =
      List(
        TemplexAR(RuneAT("__ParamRune_0", CoordTemplataType)),
        EqualsAR(
          TemplexAR(RuneAT("__ParamRune_0", CoordTemplataType)),
          TemplexAR(
            CallAT(
              NameAT("MutTStruct", TemplateTemplataType(List(CoordTemplataType), KindTemplataType)),
              List(RuneAT("T", CoordTemplataType)),
              CoordTemplataType))))

    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        rules,
        Map("__ParamRune_0" -> KindTemplataType),
        Map(),
        List(AtomSP(Some(CaptureP("x",FinalP)),Some(AbstractSP),"__ParamRune_0",None)),
        Some(List(ParamFilter(Coord(Own,StructRef2(FullName2(List(NamePart2("MutTStruct",Some(List(CoordTemplata(Coord(Share,Int2()))))))))),None))),
        true)
    inferencesD.templatasByRune("__ParamRune_0") shouldEqual
      CoordTemplata(Coord(Own,StructRef2(FullName2(List(NamePart2("MutTStruct",Some(List(CoordTemplata(Coord(Share,Int2()))))))))))
  }

  test("Test destructuring") {
    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        List(
          TemplexAR(RuneAT("__Let0_", CoordTemplataType)),
          TemplexAR(RuneAT("__Let0__Mem_0", CoordTemplataType)),
          TemplexAR(RuneAT("__Let0__Mem_1", CoordTemplataType))),
        Map("__Let0_" -> CoordTemplataType, "__Let0__Mem_0" -> CoordTemplataType, "__Let0__Mem_1" -> CoordTemplataType),
        Map(),
        List(
          AtomSP(
            None,
            None,"__Let0_",
            Some(
              List(
                Some(AtomSP(Some(CaptureP("x",FinalP)),None,"__Let0__Mem_0",None)),
                Some(AtomSP(Some(CaptureP("y",FinalP)),None,"__Let0__Mem_1",None)))))),
        Some(List(ParamFilter(Coord(Share,PackT2(List(Coord(Share,Int2()), Coord(Share,Int2())),StructRef2(FullName2(List(NamePart2("__Pack",Some(List(CoordTemplata(Coord(Share,Int2())), CoordTemplata(Coord(Share,Int2())))))))))),None))),
        true)
    inferencesD.templatasByRune("__Let0_") shouldEqual
      CoordTemplata(
        Coord(
          Share,
          PackT2(
            List(Coord(Share,Int2()), Coord(Share,Int2())),
            StructRef2(FullName2(List(NamePart2("__Pack",Some(List(CoordTemplata(Coord(Share,Int2())), CoordTemplata(Coord(Share,Int2())))))))))))
    inferencesD.templatasByRune("__Let0__Mem_0") shouldEqual
      CoordTemplata(Coord(Share,Int2()))
    inferencesD.templatasByRune("__Let0__Mem_1") shouldEqual
      CoordTemplata(Coord(Share,Int2()))
  }

  test("Test evaluating array sequence") {
    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        List(
          TemplexAR(RuneAT("__ParamRune_0", CoordTemplataType)),
          EqualsAR(
            TemplexAR(RuneAT("__ParamRune_0", CoordTemplataType)),
            TemplexAR(RepeaterSequenceAT(MutabilityAT(ImmutableP), IntAT(5),OwnershippedAT(ShareP,NameAT("Int", CoordTemplataType)), CoordTemplataType)))),
        Map("__ParamRune_0" -> CoordTemplataType),
        Map(),
        List(),
        None,
        true)
    inferencesD.templatasByRune("__ParamRune_0") shouldEqual
      CoordTemplata(Coord(Share,ArraySequenceT2(5,RawArrayT2(Coord(Share,Int2()),Immutable))))
  }

  test("Test matching array sequence as coord") {
    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        List(
          EqualsAR(
            TemplexAR(NameAT("MutArraySequenceOf4Int", CoordTemplataType)),
            TemplexAR(
              RepeaterSequenceAT(
                RuneAT("M", MutabilityTemplataType),
                RuneAT("N", IntegerTemplataType),
                RuneAT("E", CoordTemplataType),
                CoordTemplataType)))),
        Map("E" -> CoordTemplataType),
        Map(),
        List(),
        None,
        true)
    inferencesD.templatasByRune("M") shouldEqual MutabilityTemplata(Mutable)
    inferencesD.templatasByRune("N") shouldEqual IntegerTemplata(4)
    inferencesD.templatasByRune("E") shouldEqual CoordTemplata(Coord(Share,Int2()))
  }

  test("Test matching array sequence as kind") {
    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        List(
          EqualsAR(
            TemplexAR(NameAT("MutArraySequenceOf4Int", KindTemplataType)),
            TemplexAR(
              RepeaterSequenceAT(
                RuneAT("M", MutabilityTemplataType),
                RuneAT("N", IntegerTemplataType),
                RuneAT("E", CoordTemplataType),
                KindTemplataType)))),
        Map("E" -> CoordTemplataType),
        Map(),
        List(),
        None,
        true)
    inferencesD.templatasByRune("M") shouldEqual MutabilityTemplata(Mutable)
    inferencesD.templatasByRune("N") shouldEqual IntegerTemplata(4)
    inferencesD.templatasByRune("E") shouldEqual CoordTemplata(Coord(Share,Int2()))
  }

  test("Test array") {
    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        List(
          EqualsAR(
            TemplexAR(RuneAT("K", KindTemplataType)),
            TemplexAR(
              CallAT(
                NameAT("Array", TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), KindTemplataType)),
                List(MutabilityAT(MutableP), NameAT("Int", CoordTemplataType)),
                KindTemplataType))),
          EqualsAR(
            TemplexAR(RuneAT("K", KindTemplataType)),
            TemplexAR(
              CallAT(
                NameAT("Array", TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), KindTemplataType)),
                List(RuneAT("M", MutabilityTemplataType), RuneAT("T", CoordTemplataType)),
                KindTemplataType)))),
        Map("T" -> CoordTemplataType, "M" -> MutabilityTemplataType, "K" -> KindTemplataType),
        Map(),
        List(),
        None,
        true)
    inferencesD.templatasByRune("M") shouldEqual MutabilityTemplata(Mutable)
    inferencesD.templatasByRune("T") shouldEqual CoordTemplata(Coord(Share,Int2()))
  }

  test("Test evaluating isa") {
    val (InferSolveSuccess(_)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            IsaAR(
              TemplexAR(RuneAT("K", KindTemplataType)),
              TemplexAR(NameAT("MutInterface", KindTemplataType)))),
          Map("K" -> KindTemplataType),
          Map("K" -> KindTemplata(StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()))))))),
          List(),
          None,
          true)

    val (isf @ InferSolveFailure(_, _, _,_,_, _)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            IsaAR(
              TemplexAR(RuneAT("K", KindTemplataType)),
              TemplexAR(NameAT("MutInterface", KindTemplataType)))),
          Map("K" -> KindTemplataType),
          Map("K" -> KindTemplata(StructRef2(FullName2(List(NamePart2("MutSoloStruct", Some(List()))))))),
          List(),
          None,
          true)
    vassert(isf.toString.contains("Isa failed"))
  }

  test("Test matching isa") {
    val (InferSolveSuccess(_)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            EqualsAR(
              TemplexAR(RuneAT("K", KindTemplataType)),
              IsaAR(
                TemplexAR(AnonymousRuneAT(KindTemplataType)),
                TemplexAR(NameAT("MutInterface", KindTemplataType))))),
          Map("K" -> KindTemplataType),
          Map("K" -> KindTemplata(StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()))))))),
          List(),
          None,
          true)

    val (isf @ InferSolveFailure(_, _,_,_, _, _)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            IsaAR(
              TemplexAR(RuneAT("K", KindTemplataType)),
              TemplexAR(NameAT("MutInterface", KindTemplataType)))),
          Map("K" -> KindTemplataType),
          Map("K" -> KindTemplata(StructRef2(FullName2(List(NamePart2("MutSoloStruct", Some(List()))))))),
          List(),
          None,
          true)
    vassert(isf.toString.contains("Isa failed"))
  }

  test("Test ownershipped") {
    def run(sourceName: String, targetOwnership: OwnershipP): IInferSolveResult = {
      val result =
        makeCannedEvaluator().solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            EqualsAR(
              TemplexAR(RuneAT("T", CoordTemplataType)),
              TemplexAR(OwnershippedAT(targetOwnership, NameAT(sourceName, CoordTemplataType))))),
          Map("T" -> CoordTemplataType),
          Map(),
          List(AtomSP(Some(CaptureP("this",FinalP)),None,"T",None)),
          None,
          true)
      result
    }

    def expectSuccess(inferSolveResult: IInferSolveResult): Coord = {
      val InferSolveSuccess(inferencesD) = inferSolveResult
      val CoordTemplata(coord) = inferencesD.templatasByRune("T")
      coord
    }

    def expectFail(inferSolveResult: IInferSolveResult): String = {
      val isf @ InferSolveFailure(_, _, _, _, _, _) = inferSolveResult
      isf.toString
    }

    expectSuccess(run("Int", OwnP)) shouldEqual Coord(Share, Int2())
    expectSuccess(run("Int", BorrowP)) shouldEqual Coord(Share, Int2())
    expectSuccess(run("Int", ShareP)) shouldEqual Coord(Share, Int2())
    vassert(expectFail(run("Int", RawP)).contains("Expected a raw, but was a share"))

    vassert(expectFail(run("MutStruct", RawP)).contains("Expected a raw, but was an own"))
    vassert(expectFail(run("MutStruct", ShareP)).contains("Expected a share, but was an own"))
    expectSuccess(run("MutStruct", OwnP)) shouldEqual Coord(Own, StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()))))))
    expectSuccess(run("MutStruct", BorrowP)) shouldEqual Coord(Borrow, StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()))))))

    vassert(expectFail(run("MutStructBorrow", RawP)).contains("Expected a raw, but was a borrow"))
    vassert(expectFail(run("MutStructBorrow", ShareP)).contains("Expected a share, but was a borrow"))
    expectSuccess(run("MutStructBorrow", OwnP)) shouldEqual Coord(Own, StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()))))))
    expectSuccess(run("MutStructBorrow", BorrowP)) shouldEqual Coord(Borrow, StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()))))))

    vassert(expectFail(run("Void", ShareP)).contains("Expected a share, but was a raw"))
    expectSuccess(run("Void", RawP)) shouldEqual Coord(Raw, Void2())
    expectSuccess(run("Void", OwnP)) shouldEqual Coord(Raw, Void2())
    expectSuccess(run("Void", BorrowP)) shouldEqual Coord(Raw, Void2())
  }

  test("test matching ownershipped") {
    def run(sourceName: String, targetOwnership: OwnershipP): IInferSolveResult = {
      val result =
        makeCannedEvaluator().solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            EqualsAR(
              TemplexAR(NameAT(sourceName, CoordTemplataType)),
              TemplexAR(OwnershippedAT(targetOwnership, RuneAT("T", CoordTemplataType))))),
          Map("T" -> CoordTemplataType),
          Map(),
          List(),
          None,
          true)
      result
    }

    def expectSuccess(inferSolveResult: IInferSolveResult): Coord = {
      val InferSolveSuccess(inferencesD) = inferSolveResult
      val CoordTemplata(coord) = inferencesD.templatasByRune("T")
      coord
    }

    def expectFail(inferSolveResult: IInferSolveResult): String = {
      val isf @ InferSolveFailure(_, _, _, _, _, _) = inferSolveResult
      isf.toString
    }

    expectSuccess(run("Int", OwnP)) shouldEqual Coord(Share, Int2())
    expectSuccess(run("Int", BorrowP)) shouldEqual Coord(Share, Int2())
    expectSuccess(run("Int", ShareP)) shouldEqual Coord(Share, Int2())
    vassert(expectFail(run("Int", RawP)).contains("Couldn't match incoming Share against expected Raw"))

    expectSuccess(run("Void", OwnP)) shouldEqual Coord(Raw, Void2())
    expectSuccess(run("Void", BorrowP)) shouldEqual Coord(Raw, Void2())
    vassert(expectFail(run("Void", ShareP)).contains("Couldn't match incoming Raw against expected Share"))
    expectSuccess(run("Void", RawP)) shouldEqual Coord(Raw, Void2())

    vassert(expectFail(run("MutStruct", RawP)).contains("Couldn't match incoming Own against expected Raw"))
    vassert(expectFail(run("MutStruct", ShareP)).contains("Couldn't match incoming Own against expected Share"))
    // Takes the own off the incoming own coord, ends up as another own.
    expectSuccess(run("MutStruct", OwnP)) shouldEqual Coord(Own, StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()))))))
    // Tries to take the borrow off the incoming own coord... fails.
    vassert(expectFail(run("MutStruct", BorrowP)).contains("Couldn't match incoming Own against expected Borrow"))

    // Tries to take the own off the incoming borrow coord... fails.
    vassert(expectFail(run("MutStructBorrow", OwnP)).contains("Couldn't match incoming Borrow against expected Own"))
    // Takes the borrow off the incoming borrow coord, succeeds and gives us an own.
    expectSuccess(run("MutStructBorrow", BorrowP)) shouldEqual Coord(Own, StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()))))))
    vassert(expectFail(run("MutStructBorrow", ShareP)).contains("Couldn't match incoming Borrow against expected Share"))
    vassert(expectFail(run("MutStructBorrow", RawP)).contains("Couldn't match incoming Borrow against expected Raw"))
  }
}
