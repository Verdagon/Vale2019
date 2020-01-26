package net.verdagon.vale.templar.infer

import net.verdagon.vale.astronomer.{SimpleEnvironment => _, FakeState => _, _}
import net.verdagon.vale.astronomer.ruletyper.IRuleTyperEvaluatorDelegate
import net.verdagon.vale.parser._
import net.verdagon.vale.scout.{IEnvironment => _, _}
import net.verdagon.vale.{vassert, vassertSome, vfail, vimpl, scout => s}
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.templata._
import org.scalamock.scalatest.MockFactory
import net.verdagon.vale.templar.types._
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

case class SimpleEnvironment(entries: Map[AbsoluteNameA[INameA], IEnvEntry]) extends IEnvironment {
  def fullName = FullName2(List())
  def globalEnv: NamespaceEnvironment = {
    vfail()
  }
  override def getAllTemplatasWithAbsoluteName(name: AbsoluteNameA[INameA], lookupFilter: Set[ILookupContext]): List[ITemplata] = {
    entries.get(name).toList.map(EnvironmentUtils.entryToTemplata(this, _))
  }
  override def getNearestTemplataWithAbsoluteName(name: AbsoluteNameA[INameA], lookupFilter: Set[ILookupContext]): Option[ITemplata] = {
    entries.get(name).map(EnvironmentUtils.entryToTemplata(this, _))
  }
  override def getAllTemplatasWithName(name: ImpreciseNameA[IImpreciseNameStepA], lookupFilter: Set[ILookupContext]): List[ITemplata] = {
    vimpl()
  }
  override def getNearestTemplataWithName(name: ImpreciseNameA[IImpreciseNameStepA], lookupFilter: Set[ILookupContext]): Option[ITemplata] = {
    vimpl()
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

  override def structIsClosure(state: FakeState, structRef: StructRef2): Boolean = {
    vfail()
  }

  override def getSimpleInterfaceMethod(state: FakeState, interfaceRef: InterfaceRef2): Prototype2 = {
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
  override def lookupTemplata(env: SimpleEnvironment, name: AbsoluteNameA[INameA]): ITemplata = {
    vassertSome(env.getNearestTemplataWithAbsoluteName(name, Set(TemplataLookupContext)))
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

  override def lookupTemplata(env: SimpleEnvironment, name: ImpreciseNameA[IImpreciseNameStepA]): ITemplata = {
    vfail()
  }
}

class InfererTests extends FunSuite with Matchers with MockFactory {
  def makeCannedEnvironment(): SimpleEnvironment = {
    var entries = Map[AbsoluteNameA[INameA], IEnvEntry]()
    val immInterfaceName = AbsoluteNameA("", List(), TopLevelCitizenDeclarationNameA("ImmInterface", CodeLocationS(0, 0)))
    entries = entries ++ Map(immInterfaceName -> InterfaceEnvEntry(InterfaceA(immInterfaceName, ImmutableP, Some(ImmutableP), KindTemplataType, List(), Map(), List(), List())))
    val arrayName = AbsoluteNameA("", List(), TopLevelCitizenDeclarationNameA("Array", CodeLocationS(0, 0)))
    entries = entries ++ Map(arrayName -> TemplataEnvEntry(ArrayTemplateTemplata()))
    val mutTStructName = AbsoluteNameA("", List(), TopLevelCitizenDeclarationNameA("MutTStruct", CodeLocationS(0, 0)))
    entries = entries ++ Map(
        mutTStructName ->
          StructEnvEntry(
            StructA(
              mutTStructName,
              MutableP,
              Some(MutableP),
              TemplateTemplataType(List(CoordTemplataType), KindTemplataType),
              List(mutTStructName.addStep(CodeRuneA("T"))),
              Map(mutTStructName.addStep(CodeRuneA("T")) -> CoordTemplataType),
              List(),
              List())))
    val mutTInterfaceName = AbsoluteNameA("", List(), TopLevelCitizenDeclarationNameA("MutTInterface", CodeLocationS(0, 0)))
    entries = entries ++ Map(mutTInterfaceName ->
      InterfaceEnvEntry(
        InterfaceA(
          mutTInterfaceName,
          MutableP,
          Some(MutableP),
          TemplateTemplataType(List(CoordTemplataType), KindTemplataType),
          List(mutTInterfaceName.addStep(CodeRuneA("T"))),
          Map(mutTInterfaceName.addStep(CodeRuneA("T")) -> CoordTemplataType),
          List(),
          List())))
    val mutStructName = AbsoluteNameA("", List(), TopLevelCitizenDeclarationNameA("MutStruct", CodeLocationS(0, 0)))
    entries = entries ++ Map(mutStructName ->
      StructEnvEntry(StructA(mutStructName, MutableP, Some(MutableP), KindTemplataType, List(), Map(), List(), List())))
    val mutInterfaceName = AbsoluteNameA("", List(), TopLevelCitizenDeclarationNameA("MutInterface", CodeLocationS(0, 0)))
    entries = entries ++ Map(mutInterfaceName ->
      InterfaceEnvEntry(InterfaceA(mutInterfaceName, MutableP, Some(MutableP), KindTemplataType, List(), Map(), List(), List())))
    val mutStructBorrowName = AbsoluteNameA("", List(), TopLevelCitizenDeclarationNameA("MutStructBorrow", CodeLocationS(0, 0)))
    entries = entries ++ Map(mutStructBorrowName ->
      TemplataEnvEntry(CoordTemplata(Coord(Borrow, StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()), None, None))))))))
    val mutArraySequenceOf4IntName = AbsoluteNameA("", List(), TopLevelCitizenDeclarationNameA("MutArraySequenceOf4Int", CodeLocationS(0, 0)))
    entries = entries ++ Map(mutArraySequenceOf4IntName ->
      TemplataEnvEntry(KindTemplata(ArraySequenceT2(4, RawArrayT2(Coord(Share, Int2()), Mutable)))))
    val voidName = AbsoluteNameA("", List(), TopLevelCitizenDeclarationNameA("Void", CodeLocationS(0, 0)))
    entries = entries ++ Map(voidName -> TemplataEnvEntry(KindTemplata(Void2())))
    val intName = AbsoluteNameA("", List(), TopLevelCitizenDeclarationNameA("Int", CodeLocationS(0, 0)))
    entries = entries ++ Map(intName -> TemplataEnvEntry(KindTemplata(Int2())))
    SimpleEnvironment(entries)
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
            case (InterfaceTemplata(_,interfaceName(AbsoluteNameA(_, _, TopLevelCitizenDeclarationNameA("MutTInterface", _)))), List(CoordTemplata(Coord(Share, Int2())) )) => {
              (InterfaceRef2(FullName2(List(NamePart2("MutTInterface", Some(List(CoordTemplata(Coord(Share, Int2())))), None, None)))))
            }
            case (InterfaceTemplata(_,interfaceName(AbsoluteNameA(_, _, TopLevelCitizenDeclarationNameA("MutInterface", _)))), List()) => {
              (InterfaceRef2(FullName2(List(NamePart2("MutInterface", Some(List()), None, None)))))
            }
            case (InterfaceTemplata(_,interfaceName(AbsoluteNameA(_, _, TopLevelCitizenDeclarationNameA("ImmInterface", _)))), List()) => {
              (InterfaceRef2(FullName2(List(NamePart2("ImmInterface", Some(List()), None, None)))))
            }
          }
        }

        override def evaluateStructTemplata(state: FakeState, templata: StructTemplata, templateArgs: List[ITemplata]): (Kind) = {
          (templata, templateArgs) match {
            case (StructTemplata(_,structName(AbsoluteNameA(_, List(), TopLevelCitizenDeclarationNameA("MutTStruct", _)))), List(CoordTemplata(Coord(Share, Int2())) )) => {
              (StructRef2(FullName2(List(NamePart2("MutTStruct", Some(List(CoordTemplata(Coord(Share, Int2())))), None, None)))))
            }
            case (StructTemplata(_,structName(AbsoluteNameA(_, List(), TopLevelCitizenDeclarationNameA("MutStruct", _)))), List()) => {
              (StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()), None, None)))))
            }
          }
        }
        override def getInterfaceTemplataType(it: InterfaceTemplata): TemplateTemplataType = {
          it match {
            case InterfaceTemplata(_,interfaceName(AbsoluteNameA(_, List(), TopLevelCitizenDeclarationNameA("MutTInterface", _)))) => {
              TemplateTemplataType(List(CoordTemplataType), KindTemplataType)
            }
            case InterfaceTemplata(_, interfaceName(AbsoluteNameA(_, List(), TopLevelCitizenDeclarationNameA("MutInterface", _)))) => vfail()
          }
        }
        override def getStructTemplataType(it: StructTemplata): TemplateTemplataType = {
          it match {
            case StructTemplata(_,structName(AbsoluteNameA(_, List(), TopLevelCitizenDeclarationNameA("MutTStruct", _)))) => TemplateTemplataType(List(CoordTemplataType), KindTemplataType)
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
            case StructRef2(FullName2(List(NamePart2("MutTStruct",Some(List(CoordTemplata(Coord(Share, Int2())))), None, None)))) => (Set(InterfaceRef2(FullName2(List(NamePart2("MutTInterface", Some(List(CoordTemplata(Coord(Share, Int2())))), None, None))))))
            case StructRef2(FullName2(List(NamePart2("MutStruct",Some(List()), None, None)))) => (Set(InterfaceRef2(FullName2(List(NamePart2("MutInterface", Some(List()), None, None))))))
            case InterfaceRef2(FullName2(List(NamePart2("MutInterface",Some(List()), None, None)))) => (Set())
            case StructRef2(FullName2(List(NamePart2("MutSoloStruct",Some(List()), None, None)))) => (Set())
            case _ => vfail(descendantCitizenRef.toString)
          }
        }

        override def citizenIsFromTemplate(state: FakeState, citizen: CitizenRef2, template: ITemplata): (Boolean) = {
          (citizen, template) match {
            case (InterfaceRef2(FullName2(List(NamePart2("MutTInterface",Some(List(CoordTemplata(Coord(Share,Int2())))), None, None)))), InterfaceTemplata(_, interfaceName(AbsoluteNameA(_, _, TopLevelCitizenDeclarationNameA("MutTInterface", _))))) => (true)
            case (StructRef2(FullName2(List(NamePart2("MutTStruct",Some(List(CoordTemplata(Coord(Share,Int2())))), None, None)))), StructTemplata(_, structName(AbsoluteNameA(_, _, TopLevelCitizenDeclarationNameA("MutTStruct", _))))) => (true)
            case (StructRef2(FullName2(List(NamePart2("MutTStruct",Some(List(CoordTemplata(Coord(Share,Int2())))), None, None)))), InterfaceTemplata(_, interfaceName(AbsoluteNameA(_, _, TopLevelCitizenDeclarationNameA("MutTInterface", _))))) => (false)
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
            TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("__C")), CoordTemplataType)),
            EqualsAR(
              TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("__C")), CoordTemplataType)),
              TemplexAR(OwnershippedAT(BorrowP,NameAT(ImpreciseNameA(List(), CodeTypeNameA("ImmInterface")), CoordTemplataType))))),
          Map(AbsoluteNameA("", List(), CodeRuneA("__C")) -> CoordTemplataType),
          Map(),
          List(),
          None,
          true)

    vassert(
      inferences.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("__C"))) ==
        CoordTemplata(Coord(Share, InterfaceRef2(FullName2(List(NamePart2("ImmInterface", Some(List()), None, None)))))))
  }

  test("Can infer coord rune from an incoming kind") {
    val (isf @ InferSolveFailure(_, _, _,_,_, _)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("C")), CoordTemplataType))),
          Map(AbsoluteNameA("", List(), CodeRuneA("C")) -> CoordTemplataType),
          Map(AbsoluteNameA("", List(), CodeRuneA("C")) -> KindTemplata(InterfaceRef2(FullName2(List(NamePart2("ImmInterface",Some(List(KindTemplata(Int2()))), None, None)))))),
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
          List(EqualsAR(TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("C")), CoordTemplataType)), TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("A")), KindTemplataType)))),
          Map(AbsoluteNameA("", List(), CodeRuneA("A")) -> KindTemplataType),
          Map(AbsoluteNameA("", List(), CodeRuneA("A")) -> KindTemplata(Int2())),
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
              TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("C")), CoordTemplataType)),
              CallAR("toRef", List(TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("A")), KindTemplataType))), CoordTemplataType))),
          Map(AbsoluteNameA("", List(), CodeRuneA("C")) -> CoordTemplataType, AbsoluteNameA("", List(), CodeRuneA("A")) -> KindTemplataType),
          Map(AbsoluteNameA("", List(), CodeRuneA("A")) -> KindTemplata(Int2())),
          List(),
          None,
          true)

    conclusions.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("C"))) shouldEqual CoordTemplata(Coord(Share, Int2()))
  }

  test("Can explicitly coerce from kind to coord 2") {
    val (InferSolveSuccess(conclusions)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(0)), CoordTemplataType)),
            EqualsAR(TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(0)), CoordTemplataType)),TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("Int")), CoordTemplataType)))),
          Map(AbsoluteNameA("", List(), ImplicitRuneA(0)) -> CoordTemplataType),
          Map(),
          List(),
          None,
          true)

    conclusions.templatasByRune(AbsoluteNameA("", List(), ImplicitRuneA(0))) shouldEqual CoordTemplata(Coord(Share, Int2()))
  }

  test("Can match KindTemplataType against StructEnvEntry / StructTemplata") {
    val (InferSolveSuccess(conclusions)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            EqualsAR(
              TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("__RetRune")), CoordTemplataType)),
              CallAR("toRef",List(TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("MutStruct")), KindTemplataType))), TemplateTemplataType(List(KindTemplataType), CoordTemplataType)))),
          Map(AbsoluteNameA("", List(), CodeRuneA("__RetRune")) -> CoordTemplataType),
          Map(),
          List(),
          None,
          true)

    conclusions.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("__RetRune"))) shouldEqual CoordTemplata(Coord(Own, StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()), None, None))))))
  }

  test("Can infer from simple rules") {
    val (InferSolveSuccess(inferences)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(0)), CoordTemplataType)),
            EqualsAR(TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(0)), CoordTemplataType)),CallAR("toRef", List(TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("Int")), KindTemplataType))), CoordTemplataType))),
          Map(AbsoluteNameA("", List(), ImplicitRuneA(0)) -> CoordTemplataType),
          Map(),
          List(),
          None,
          true)

    vassert(inferences.templatasByRune(AbsoluteNameA("", List(), ImplicitRuneA(0))) == CoordTemplata(Coord(Share, Int2())))
  }

  test("Can infer templata from CallAT") {
    val (InferSolveSuccess(inferences)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            EqualsAR(
              TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("X")), KindTemplataType)),
              TemplexAR(CallAT(NameAT(ImpreciseNameA(List(), CodeTypeNameA("MutTInterface")), TemplateTemplataType(List(CoordTemplataType), KindTemplataType)),List(RuneAT(AbsoluteNameA("", List(), CodeRuneA("T")), CoordTemplataType)), KindTemplataType)))),
          Map(AbsoluteNameA("", List(), CodeRuneA("X")) -> KindTemplataType, AbsoluteNameA("", List(), CodeRuneA("T")) -> CoordTemplataType),
          Map(AbsoluteNameA("", List(), CodeRuneA("X")) -> KindTemplata(InterfaceRef2(FullName2(List(NamePart2("MutTInterface",Some(List(CoordTemplata(Coord(Share, Int2())))), None, None)))))),
          List(),
          None,
          true)

    vassert(inferences.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("T"))) == CoordTemplata(Coord(Share, Int2())))
  }

  test("Can conjure an owning coord from a borrow coord") {
    val (InferSolveSuccess(inferences)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("T")), CoordTemplataType)),
            TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(1337)), KindTemplataType)),
            EqualsAR(
              TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("T")), CoordTemplataType)),
              ComponentsAR(
                CoordTemplataType,
                List(TemplexAR(OwnershipAT(OwnP)), TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(1337)), KindTemplataType))))),
            TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(0)), CoordTemplataType)),
            EqualsAR(
              TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(0)), CoordTemplataType)),
              ComponentsAR(
                CoordTemplataType,
                List(TemplexAR(OwnershipAT(BorrowP)), TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(1337)), KindTemplataType)))))),
          Map(
            AbsoluteNameA("", List(), ImplicitRuneA(1337)) -> KindTemplataType,
            AbsoluteNameA("", List(), ImplicitRuneA(0)) -> CoordTemplataType),
          Map(),
          List(AtomAP(CaptureA(AbsoluteNameA("", List(), CodeVarNameA("m")),FinalP),None,AbsoluteNameA("", List(), ImplicitRuneA(0)),None)),
          Some(List(ParamFilter(Coord(Borrow,InterfaceRef2(FullName2(List(NamePart2("MutInterface", Some(List()), None, None))))),None))),
          true)

    vassert(inferences.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("T"))) == CoordTemplata(Coord(Own,InterfaceRef2(FullName2(List(NamePart2("MutInterface", Some(List()), None, None)))))))
  }

  test("Rune 0 upcasts to right type, simple") {
    val (InferSolveSuccess(inferences)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("__Let0_")), CoordTemplataType)),
            EqualsAR(
              TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("__Let0_")), CoordTemplataType)),
              CallAR("toRef", List(TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("MutInterface")), KindTemplataType))), CoordTemplataType))),
          Map(AbsoluteNameA("", List(), CodeRuneA("__Let0_")) -> KindTemplataType),
          Map(),
          List(AtomAP(CaptureA(AbsoluteNameA("", List(), CodeVarNameA("x")),FinalP),None,AbsoluteNameA("", List(), CodeRuneA("__Let0_")),None)),
          Some(List(ParamFilter(Coord(Own,StructRef2(FullName2(List(NamePart2("MutStruct",Some(List()), None, None))))),None))),
          true)

    vassert(inferences.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("__Let0_"))) == CoordTemplata(Coord(Own, InterfaceRef2(FullName2(List(NamePart2("MutInterface", Some(List()), None, None)))))))
  }

  test("Rune 0 upcasts to right type templated") {
    val (InferSolveSuccess(inferences)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("__Let0_")), CoordTemplataType)),
            EqualsAR(
              TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("__Let0_")), CoordTemplataType)),
              CallAR(
                "toRef",
                List(
                  TemplexAR(
                    CallAT(
                      NameAT(ImpreciseNameA(List(), CodeTypeNameA("MutTInterface")), TemplateTemplataType(List(CoordTemplataType), KindTemplataType)),
                      List(RuneAT(AbsoluteNameA("", List(), CodeRuneA("T")), CoordTemplataType)),
                      KindTemplataType))),
                CoordTemplataType))),
          Map(AbsoluteNameA("", List(), CodeRuneA("__Let0_")) -> KindTemplataType, AbsoluteNameA("", List(), CodeRuneA("T")) -> CoordTemplataType),
          Map(),
          List(AtomAP(CaptureA(AbsoluteNameA("", List(), CodeVarNameA("x")),FinalP),None,AbsoluteNameA("", List(), CodeRuneA("__Let0_")),None)),
          Some(List(ParamFilter(Coord(Own,StructRef2(FullName2(List(NamePart2("MutTStruct",Some(List(CoordTemplata(Coord(Share, Int2())))), None, None))))),None))),
          true)

    vassert(inferences.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("__Let0_"))) == CoordTemplata(Coord(Own, InterfaceRef2(FullName2(List(NamePart2("MutTInterface", Some(List(CoordTemplata(Coord(Share, Int2())))), None, None)))))))
    vassert(inferences.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("T"))) == CoordTemplata(Coord(Share, Int2())))
  }

  test("Tests destructor") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        EqualsAR(
          TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("T")), CoordTemplataType)),
          ComponentsAR(
            CoordTemplataType,
            List(
              OrAR(List(TemplexAR(OwnershipAT(OwnP)), TemplexAR(OwnershipAT(ShareP)))),
              CallAR("passThroughIfConcrete",List(TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(0)), KindTemplataType))), KindTemplataType)))),
        EqualsAR(TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("V")), CoordTemplataType)),CallAR("toRef",List(TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("Void")),KindTemplataType))), CoordTemplataType)))
    val atoms =
      List(AtomAP(CaptureA(AbsoluteNameA("", List(), CodeVarNameA("this")),FinalP),None,AbsoluteNameA("", List(), CodeRuneA("T")),None))

    val solve =
      (paramFilter: ParamFilter) => {
        makeCannedEvaluator().solve(
          makeCannedEnvironment(),
          FakeState(),
          rules,
          Map(AbsoluteNameA("", List(), CodeRuneA("V")) -> CoordTemplataType, AbsoluteNameA("", List(), CodeRuneA("T")) -> CoordTemplataType),
          Map(),
          atoms,
          Some(List(paramFilter)),
          true)
      }

    // Test that it does match a pack
    val packCoord = Coord(Share,PackT2(List(),StructRef2(FullName2(List(NamePart2("__Pack",Some(List()), None, None))))))
    val (InferSolveSuccess(inferencesA)) = solve(ParamFilter(packCoord,None))
    vassert(inferencesA.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("T"))) == CoordTemplata(packCoord))

    // Test that it does match a struct
    val structCoord = Coord(Own,StructRef2(FullName2(List(NamePart2("MutStruct",Some(List()), None, None)))))
    val (InferSolveSuccess(inferencesD)) = solve(ParamFilter(structCoord,None))
    vassert(inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("T"))) == CoordTemplata(structCoord))

    // Test that it doesn't match an int
    val intCoord = Coord(Share,Int2())
    val (isfE @ InferSolveFailure(_, _,_,_, _, _)) = solve(ParamFilter(intCoord,None))
    vassert(isfE.toString.contains("Bad arguments to passThroughIfConcrete"))

    // Test that it doesn't match an interface
    val interfaceCoord = Coord(Own,InterfaceRef2(FullName2(List(NamePart2("MutInterface",Some(List()), None, None)))))
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
          TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("T")), CoordTemplataType)),
          ComponentsAR(
            CoordTemplataType,
            List(
              OrAR(List(TemplexAR(OwnershipAT(OwnP)), TemplexAR(OwnershipAT(ShareP)))),
              CallAR("passThroughIfInterface",List(TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(0)), KindTemplataType))), KindTemplataType)))),
        EqualsAR(
          TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("V")), CoordTemplataType)),
          CallAR("toRef",List(TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("Void")), KindTemplataType))), CoordTemplataType)))
    val atoms =
      List(AtomAP(CaptureA(AbsoluteNameA("", List(), CodeVarNameA("this")),FinalP),None,AbsoluteNameA("", List(), CodeRuneA("T")),None))

    val solve =
      (paramFilter: ParamFilter) => {
        makeCannedEvaluator().solve(
          makeCannedEnvironment(),
          FakeState(),
          rules,
          Map(AbsoluteNameA("", List(), CodeRuneA("T")) -> CoordTemplataType, AbsoluteNameA("", List(), CodeRuneA("V")) -> CoordTemplataType),
          Map(),
          atoms,
          Some(List(paramFilter)),
          true)
      }

    // Test that it does match an interface
    val interfaceCoord = Coord(Own,InterfaceRef2(FullName2(List(NamePart2("MutInterface",Some(List()), None, None)))))
    val (InferSolveSuccess(inferencesD)) = solve(ParamFilter(interfaceCoord,None))
    vassert(inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("T"))) == CoordTemplata(interfaceCoord))

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
          TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("T")), CoordTemplataType)),
          ComponentsAR(
            CoordTemplataType,
            List(
              OrAR(List(TemplexAR(OwnershipAT(OwnP)), TemplexAR(OwnershipAT(ShareP)))),
              CallAR("passThroughIfStruct",List(TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(0)), KindTemplataType))), KindTemplataType)))))
    val atoms =
      List(AtomAP(CaptureA(AbsoluteNameA("", List(), CodeVarNameA("this")), FinalP),None,AbsoluteNameA("", List(), CodeRuneA("T")),None))

    val solve =
      (paramFilter: ParamFilter) => {
        makeCannedEvaluator().solve(
          makeCannedEnvironment(),
          FakeState(),
          rules,
          Map(AbsoluteNameA("", List(), CodeRuneA("T")) -> CoordTemplataType),
          Map(),
          atoms,
          Some(List(paramFilter)),
          true)
      }

    // Test that it does match a struct
    val structCoord = Coord(Own,StructRef2(FullName2(List(NamePart2("MutStruct",Some(List()), None, None)))))
    val (InferSolveSuccess(inferencesD)) = solve(ParamFilter(structCoord,None))
    vassert(inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("T"))) == CoordTemplata(structCoord))

    // Test that it doesn't match an int
    val intCoord = Coord(Share,Int2())
    val (isfE @ InferSolveFailure(_, _, _,_,_, _)) = solve(ParamFilter(intCoord,None))
    vassert(isfE.toString.contains("Bad arguments to passThroughIfStruct"))

    // Test that it doesn't match an interface
    val interfaceCoord = Coord(Own,InterfaceRef2(FullName2(List(NamePart2("MutInterface",Some(List()), None, None)))))
    val (isfF @ InferSolveFailure(_, _, _,_,_, _)) = solve(ParamFilter(interfaceCoord,None))
    vassert(isfF.toString.contains("Bad arguments to passThroughIfStruct"))

    // Test that it doesn't match an pack
    val packCoord = Coord(Share,PackT2(List(),StructRef2(FullName2(List(NamePart2("__Pack",Some(List()), None, None))))))
    val (isfG @ InferSolveFailure(_, _, _,_,_, _)) = solve(ParamFilter(packCoord,None))
    vassert(isfG.toString.contains("Bad arguments to passThroughIfStruct"))
  }

  test("Test coercing template call result") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(0)), CoordTemplataType)),
        EqualsAR(
          TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(0)), CoordTemplataType)),
          TemplexAR(
            CallAT(
              NameAT(ImpreciseNameA(List(), CodeTypeNameA("MutTStruct")), TemplateTemplataType(List(CoordTemplataType), KindTemplataType)),
              List(NameAT(ImpreciseNameA(List(), CodeTypeNameA("Int")), CoordTemplataType)),
              CoordTemplataType))))
    val atoms =
      List(AtomAP(CaptureA(AbsoluteNameA("", List(), CodeVarNameA("this")),FinalP),None,AbsoluteNameA("", List(), CodeRuneA("T")),None))

    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        rules,
        Map(AbsoluteNameA("", List(), ImplicitRuneA(0)) -> CoordTemplataType),
        Map(),
        atoms,
        None,
        true)

    inferencesD.templatasByRune(AbsoluteNameA("", List(), ImplicitRuneA(0))) shouldEqual
      CoordTemplata(Coord(Own,StructRef2(FullName2(List(NamePart2("MutTStruct",Some(List(CoordTemplata(Coord(Share,Int2())))), None, None))))))
  }


  test("Test result of a CallAT can coerce to coord") {
    val rules =
      List(
        TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("__Par0")), CoordTemplataType)),
        EqualsAR(TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("__Par0")), CoordTemplataType)),TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("MutStruct")), CoordTemplataType))))
    val atoms =
      List(AtomAP(CaptureA(AbsoluteNameA("", List(), CodeVarNameA("this")),FinalP),None,AbsoluteNameA("", List(), CodeRuneA("T")),None))

    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        rules,
        Map(AbsoluteNameA("", List(), CodeRuneA("__Par0")) -> CoordTemplataType),
        Map(),
        atoms,
        None,
        true)
    inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("__Par0"))) shouldEqual
      CoordTemplata(Coord(Own,StructRef2(FullName2(List(NamePart2("MutStruct",Some(List()), None, None))))))
  }

  test("Matching a CoordTemplataType onto a CallAT") {
    val rules =
      List(
        TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(0)), CoordTemplataType)),
        EqualsAR(
          TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(0)), CoordTemplataType)),
          TemplexAR(
            CallAT(
              NameAT(ImpreciseNameA(List(), CodeTypeNameA("MutTStruct")), TemplateTemplataType(List(CoordTemplataType), KindTemplataType)),
              List(RuneAT(AbsoluteNameA("", List(), CodeRuneA("T")), CoordTemplataType)),
              CoordTemplataType))))

    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        rules,
        Map(AbsoluteNameA("", List(), ImplicitRuneA(0)) -> KindTemplataType),
        Map(),
        List(AtomAP(CaptureA(AbsoluteNameA("", List(), CodeVarNameA("x")),FinalP),Some(AbstractAP),AbsoluteNameA("", List(), ImplicitRuneA(0)),None)),
        Some(List(ParamFilter(Coord(Own,StructRef2(FullName2(List(NamePart2("MutTStruct",Some(List(CoordTemplata(Coord(Share,Int2())))), None, None))))),None))),
        true)
    inferencesD.templatasByRune(AbsoluteNameA("", List(), ImplicitRuneA(0))) shouldEqual
      CoordTemplata(Coord(Own,StructRef2(FullName2(List(NamePart2("MutTStruct",Some(List(CoordTemplata(Coord(Share,Int2())))), None, None))))))
  }

  test("Test destructuring") {
    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        List(
          TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("__Let0_")), CoordTemplataType)),
          TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("__Let0__Mem_0")), CoordTemplataType)),
          TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("__Let0__Mem_1")), CoordTemplataType))),
        Map(AbsoluteNameA("", List(), CodeRuneA("__Let0_")) -> CoordTemplataType, AbsoluteNameA("", List(), CodeRuneA("__Let0__Mem_0")) -> CoordTemplataType, AbsoluteNameA("", List(), CodeRuneA("__Let0__Mem_1")) -> CoordTemplataType),
        Map(),
        List(
          AtomAP(
            CaptureA(AbsoluteNameA("", List(), CodeVarNameA("a")), FinalP),
            None,
            AbsoluteNameA("", List(), CodeRuneA("__Let0_")),
            Some(
              List(
                AtomAP(CaptureA(AbsoluteNameA("", List(), CodeVarNameA("x")), FinalP),None,AbsoluteNameA("", List(), CodeRuneA("__Let0__Mem_0")),None),
                AtomAP(CaptureA(AbsoluteNameA("", List(), CodeVarNameA("y")), FinalP),None,AbsoluteNameA("", List(), CodeRuneA("__Let0__Mem_1")),None))))),
        Some(List(ParamFilter(Coord(Share,PackT2(List(Coord(Share,Int2()), Coord(Share,Int2())),StructRef2(FullName2(List(NamePart2("__Pack",Some(List(CoordTemplata(Coord(Share,Int2())), CoordTemplata(Coord(Share,Int2())))), None, None)))))),None))),
        true)
    inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("__Let0_"))) shouldEqual
      CoordTemplata(
        Coord(
          Share,
          PackT2(
            List(Coord(Share,Int2()), Coord(Share,Int2())),
            StructRef2(FullName2(List(NamePart2("__Pack",Some(List(CoordTemplata(Coord(Share,Int2())), CoordTemplata(Coord(Share,Int2())))), None, None)))))))
    inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("__Let0__Mem_0"))) shouldEqual
      CoordTemplata(Coord(Share,Int2()))
    inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("__Let0__Mem_1"))) shouldEqual
      CoordTemplata(Coord(Share,Int2()))
  }

  test("Test evaluating array sequence") {
    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        List(
          TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(0)), CoordTemplataType)),
          EqualsAR(
            TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(0)), CoordTemplataType)),
            TemplexAR(RepeaterSequenceAT(MutabilityAT(ImmutableP), IntAT(5),OwnershippedAT(ShareP,NameAT(ImpreciseNameA(List(), CodeTypeNameA("Int")), CoordTemplataType)), CoordTemplataType)))),
        Map(AbsoluteNameA("", List(), ImplicitRuneA(0)) -> CoordTemplataType),
        Map(),
        List(),
        None,
        true)
    inferencesD.templatasByRune(AbsoluteNameA("", List(), ImplicitRuneA(0))) shouldEqual
      CoordTemplata(Coord(Share,ArraySequenceT2(5,RawArrayT2(Coord(Share,Int2()),Immutable))))
  }

  test("Test matching array sequence as coord") {
    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        List(
          EqualsAR(
            TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("MutArraySequenceOf4Int")), CoordTemplataType)),
            TemplexAR(
              RepeaterSequenceAT(
                RuneAT(AbsoluteNameA("", List(), CodeRuneA("M")), MutabilityTemplataType),
                RuneAT(AbsoluteNameA("", List(), CodeRuneA("N")), IntegerTemplataType),
                RuneAT(AbsoluteNameA("", List(), CodeRuneA("E")), CoordTemplataType),
                CoordTemplataType)))),
        Map(AbsoluteNameA("", List(), CodeRuneA("E")) -> CoordTemplataType),
        Map(),
        List(),
        None,
        true)
    inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("M"))) shouldEqual MutabilityTemplata(Mutable)
    inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("N"))) shouldEqual IntegerTemplata(4)
    inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("E"))) shouldEqual CoordTemplata(Coord(Share,Int2()))
  }

  test("Test matching array sequence as kind") {
    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        List(
          EqualsAR(
            TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("MutArraySequenceOf4Int")), KindTemplataType)),
            TemplexAR(
              RepeaterSequenceAT(
                RuneAT(AbsoluteNameA("", List(), CodeRuneA("M")), MutabilityTemplataType),
                RuneAT(AbsoluteNameA("", List(), CodeRuneA("N")), IntegerTemplataType),
                RuneAT(AbsoluteNameA("", List(), CodeRuneA("E")), CoordTemplataType),
                KindTemplataType)))),
        Map(AbsoluteNameA("", List(), CodeRuneA("E")) -> CoordTemplataType),
        Map(),
        List(),
        None,
        true)
    inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("M"))) shouldEqual MutabilityTemplata(Mutable)
    inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("N"))) shouldEqual IntegerTemplata(4)
    inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("E"))) shouldEqual CoordTemplata(Coord(Share,Int2()))
  }

  test("Test array") {
    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        List(
          EqualsAR(
            TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("K")), KindTemplataType)),
            TemplexAR(
              CallAT(
                NameAT(ImpreciseNameA(List(), CodeTypeNameA("Array")), TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), KindTemplataType)),
                List(MutabilityAT(MutableP), NameAT(ImpreciseNameA(List(), CodeTypeNameA("Int")), CoordTemplataType)),
                KindTemplataType))),
          EqualsAR(
            TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("K")), KindTemplataType)),
            TemplexAR(
              CallAT(
                NameAT(ImpreciseNameA(List(), CodeTypeNameA("Array")), TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), KindTemplataType)),
                List(RuneAT(AbsoluteNameA("", List(), CodeRuneA("M")), MutabilityTemplataType), RuneAT(AbsoluteNameA("", List(), CodeRuneA("T")), CoordTemplataType)),
                KindTemplataType)))),
        Map(AbsoluteNameA("", List(), CodeRuneA("T")) -> CoordTemplataType, AbsoluteNameA("", List(), CodeRuneA("M")) -> MutabilityTemplataType, AbsoluteNameA("", List(), CodeRuneA("K")) -> KindTemplataType),
        Map(),
        List(),
        None,
        true)
    inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("M"))) shouldEqual MutabilityTemplata(Mutable)
    inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("T"))) shouldEqual CoordTemplata(Coord(Share,Int2()))
  }

  test("Test evaluating isa") {
    val (InferSolveSuccess(_)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            IsaAR(
              TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("K")), KindTemplataType)),
              TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("MutInterface")), KindTemplataType)))),
          Map(AbsoluteNameA("", List(), CodeRuneA("K")) -> KindTemplataType),
          Map(AbsoluteNameA("", List(), CodeRuneA("K")) -> KindTemplata(StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()), None, None)))))),
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
              TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("K")), KindTemplataType)),
              TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("MutInterface")), KindTemplataType)))),
          Map(AbsoluteNameA("", List(), CodeRuneA("K")) -> KindTemplataType),
          Map(AbsoluteNameA("", List(), CodeRuneA("K")) -> KindTemplata(StructRef2(FullName2(List(NamePart2("MutSoloStruct", Some(List()), None, None)))))),
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
              TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("K")), KindTemplataType)),
              IsaAR(
                TemplexAR(RuneAT(AbsoluteNameA("", List(), ImplicitRuneA(0)), KindTemplataType)),
                TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("MutInterface")), KindTemplataType))))),
          Map(AbsoluteNameA("", List(), CodeRuneA("K")) -> KindTemplataType),
          Map(AbsoluteNameA("", List(), CodeRuneA("K")) -> KindTemplata(StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()), None, None)))))),
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
              TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("K")), KindTemplataType)),
              TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA("MutInterface")), KindTemplataType)))),
          Map(AbsoluteNameA("", List(), CodeRuneA("K")) -> KindTemplataType),
          Map(AbsoluteNameA("", List(), CodeRuneA("K")) -> KindTemplata(StructRef2(FullName2(List(NamePart2("MutSoloStruct", Some(List()), None, None)))))),
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
              TemplexAR(RuneAT(AbsoluteNameA("", List(), CodeRuneA("T")), CoordTemplataType)),
              TemplexAR(OwnershippedAT(targetOwnership, NameAT(ImpreciseNameA(List(), CodeTypeNameA(sourceName)), CoordTemplataType))))),
          Map(AbsoluteNameA("", List(), CodeRuneA("T")) -> CoordTemplataType),
          Map(),
          List(AtomAP(CaptureA(AbsoluteNameA("", List(), CodeVarNameA("this")),FinalP),None,AbsoluteNameA("", List(), CodeRuneA("T")),None)),
          None,
          true)
      result
    }

    def expectSuccess(inferSolveResult: IInferSolveResult): Coord = {
      val InferSolveSuccess(inferencesD) = inferSolveResult
      val CoordTemplata(coord) = inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("T")))
      coord
    }

    def expectFail(inferSolveResult: IInferSolveResult): String = {
      val isf @ InferSolveFailure(_, _, _, _, _, _) = inferSolveResult
      isf.toString
    }

    expectSuccess(run("Int", OwnP)) shouldEqual Coord(Share, Int2())
    expectSuccess(run("Int", BorrowP)) shouldEqual Coord(Share, Int2())
    expectSuccess(run("Int", ShareP)) shouldEqual Coord(Share, Int2())

    vassert(expectFail(run("MutStruct", ShareP)).contains("Expected a share, but was an own"))
    expectSuccess(run("MutStruct", OwnP)) shouldEqual Coord(Own, StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()), None, None)))))
    expectSuccess(run("MutStruct", BorrowP)) shouldEqual Coord(Borrow, StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()), None, None)))))

    vassert(expectFail(run("MutStructBorrow", ShareP)).contains("Expected a share, but was a borrow"))
    expectSuccess(run("MutStructBorrow", OwnP)) shouldEqual Coord(Own, StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()), None, None)))))
    expectSuccess(run("MutStructBorrow", BorrowP)) shouldEqual Coord(Borrow, StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()), None, None)))))

    expectSuccess(run("Void", ShareP)) shouldEqual Coord(Share, Void2())
    expectSuccess(run("Void", OwnP)) shouldEqual Coord(Share, Void2())
    expectSuccess(run("Void", BorrowP)) shouldEqual Coord(Share, Void2())
  }

  test("test matching ownershipped") {
    def run(sourceName: String, targetOwnership: OwnershipP): IInferSolveResult = {
      val result =
        makeCannedEvaluator().solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            EqualsAR(
              TemplexAR(NameAT(ImpreciseNameA(List(), CodeTypeNameA(sourceName)), CoordTemplataType)),
              TemplexAR(OwnershippedAT(targetOwnership, RuneAT(AbsoluteNameA("", List(), CodeRuneA("T")), CoordTemplataType))))),
          Map(AbsoluteNameA("", List(), CodeRuneA("T")) -> CoordTemplataType),
          Map(),
          List(),
          None,
          true)
      result
    }

    def expectSuccess(inferSolveResult: IInferSolveResult): Coord = {
      val InferSolveSuccess(inferencesD) = inferSolveResult
      val CoordTemplata(coord) = inferencesD.templatasByRune(AbsoluteNameA("", List(), CodeRuneA("T")))
      coord
    }

    def expectFail(inferSolveResult: IInferSolveResult): String = {
      val isf @ InferSolveFailure(_, _, _, _, _, _) = inferSolveResult
      isf.toString
    }

    expectSuccess(run("Int", OwnP)) shouldEqual Coord(Share, Int2())
    expectSuccess(run("Int", BorrowP)) shouldEqual Coord(Share, Int2())
    expectSuccess(run("Int", ShareP)) shouldEqual Coord(Share, Int2())

    expectSuccess(run("Void", OwnP)) shouldEqual Coord(Share, Void2())
    expectSuccess(run("Void", BorrowP)) shouldEqual Coord(Share, Void2())
    expectSuccess(run("Void", ShareP)) shouldEqual Coord(Share, Void2())

    vassert(expectFail(run("MutStruct", ShareP)).contains("Couldn't match incoming Own against expected Share"))
    // Takes the own off the incoming own coord, ends up as another own.
    expectSuccess(run("MutStruct", OwnP)) shouldEqual Coord(Own, StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()), None, None)))))
    // Tries to take the borrow off the incoming own coord... fails.
    vassert(expectFail(run("MutStruct", BorrowP)).contains("Couldn't match incoming Own against expected Borrow"))

    // Tries to take the own off the incoming borrow coord... fails.
    vassert(expectFail(run("MutStructBorrow", OwnP)).contains("Couldn't match incoming Borrow against expected Own"))
    // Takes the borrow off the incoming borrow coord, succeeds and gives us an own.
    expectSuccess(run("MutStructBorrow", BorrowP)) shouldEqual Coord(Own, StructRef2(FullName2(List(NamePart2("MutStruct", Some(List()), None, None)))))
    vassert(expectFail(run("MutStructBorrow", ShareP)).contains("Couldn't match incoming Borrow against expected Share"))
  }
}
