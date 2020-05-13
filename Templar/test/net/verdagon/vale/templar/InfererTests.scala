package net.verdagon.vale.templar.infer

import net.verdagon.vale.astronomer.{FakeState => _, SimpleEnvironment => _, _}
import net.verdagon.vale.astronomer.ruletyper.IRuleTyperEvaluatorDelegate
import net.verdagon.vale.parser._
import net.verdagon.vale.scout.{IEnvironment => _, _}
import net.verdagon.vale.templar.{CodeRune2, FullName2, IName2, ImplicitRune2, CitizenName2}
import net.verdagon.vale.{vassert, vassertSome, vfail, vimpl, scout => s}
import net.verdagon.vale.templar.env._
import net.verdagon.vale.templar.infer.{InfererEquator, InfererEvaluator}
import net.verdagon.vale.templar.infer.infer.{IInferSolveResult, InferSolveFailure, InferSolveSuccess}
import net.verdagon.vale.templar.templata._
//import org.scalamock.scalatest.MockFactory
import net.verdagon.vale.templar.types._
import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.List

case class FakeEnv()
case class FakeState()

object InfererTestUtils {
  def getMutability(kind: Kind): Mutability = {
    kind match {
      case Int2() => Immutable
      case StructRef2(FullName2(_, CitizenName2(humanName, _))) if humanName.startsWith("Imm") => Immutable
      case StructRef2(FullName2(_, CitizenName2(humanName, _))) if humanName.startsWith("Mut") => Mutable
      case InterfaceRef2(FullName2(_, CitizenName2(humanName, _))) if humanName.startsWith("Imm") => Immutable
      case InterfaceRef2(FullName2(_, CitizenName2(humanName, _))) if humanName.startsWith("Mut") => Mutable
      case ArraySequenceT2(_, RawArrayT2(_, mutability)) => mutability
      case UnknownSizeArrayT2(RawArrayT2(_, mutability)) => mutability
    }
  }
}

case class SimpleEnvironment(entries: Map[INameA, IEnvEntry]) extends IEnvironment {
  def fullName = FullName2(List(), CitizenName2("SimpleEnv", List()))
  def globalEnv: NamespaceEnvironment[IName2] = {
    vfail()
  }
  override def getAllTemplatasWithAbsoluteNameA(name: INameA, lookupFilter: Set[ILookupContext]): List[ITemplata] = {
    entries.get(name).toList.map(EnvironmentUtils.entryToTemplata(this, _))
  }
  override def getNearestTemplataWithAbsoluteNameA(name: INameA, lookupFilter: Set[ILookupContext]): Option[ITemplata] = {
    entries.get(name).map(EnvironmentUtils.entryToTemplata(this, _))
  }
  override def getAllTemplatasWithName(name: IImpreciseNameStepA, lookupFilter: Set[ILookupContext]): List[ITemplata] = {
    vimpl()
  }
  override def getNearestTemplataWithName(name: IImpreciseNameStepA, lookupFilter: Set[ILookupContext]): Option[ITemplata] = {
    vimpl()
  }

  override def getAllTemplatasWithAbsoluteName2(name: IName2, lookupFilter: Set[ILookupContext]): List[ITemplata] = {
    vimpl()
  }

  override def getNearestTemplataWithAbsoluteName2(name: IName2, lookupFilter: Set[ILookupContext]): Option[ITemplata] = {
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

  override def lookupTemplata(env: SimpleEnvironment, rune: IName2): ITemplata = {
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
  override def lookupTemplata(env: SimpleEnvironment, name: IName2): ITemplata = {
    vassertSome(env.getNearestTemplataWithAbsoluteName2(name, Set(TemplataLookupContext)))
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

  override def lookupTemplataImprecise(env: SimpleEnvironment, name: IImpreciseNameStepA): ITemplata = {
    vfail()
  }
}

class InfererTests extends FunSuite with Matchers {
  def makeCannedEnvironment(): SimpleEnvironment = {
    var entries = Map[INameA, IEnvEntry]()
    val immInterfaceName = TopLevelCitizenDeclarationNameA("ImmInterface", CodeLocationS(0, 0))
    entries = entries ++ Map(immInterfaceName -> InterfaceEnvEntry(InterfaceA(immInterfaceName, ImmutableP, Some(ImmutableP), KindTemplataType, Set(), List(), Set(), Map(), List(), List())))
    val arrayName = TopLevelCitizenDeclarationNameA("Array", CodeLocationS(0, 0))
    entries = entries ++ Map(arrayName -> TemplataEnvEntry(ArrayTemplateTemplata()))
    val mutTStructName = TopLevelCitizenDeclarationNameA("MutTStruct", CodeLocationS(0, 0))
    entries = entries ++ Map(
        mutTStructName ->
          StructEnvEntry(
            StructA(
              mutTStructName,
              MutableP,
              Some(MutableP),
              TemplateTemplataType(List(CoordTemplataType), KindTemplataType),
              Set(),
              List(CodeRuneA("T")),
              Set(CodeRuneA("T")),
              Map(CodeRuneA("T") -> CoordTemplataType),
              List(),
              List())))
    val mutTInterfaceName = TopLevelCitizenDeclarationNameA("MutTInterface", CodeLocationS(0, 0))
    entries = entries ++ Map(mutTInterfaceName ->
      InterfaceEnvEntry(
        InterfaceA(
          mutTInterfaceName,
          MutableP,
          Some(MutableP),
          TemplateTemplataType(List(CoordTemplataType), KindTemplataType),
          Set(),
          List(CodeRuneA("T")),
          Set(CodeRuneA("T")),
          Map(CodeRuneA("T") -> CoordTemplataType),
          List(),
          List())))
    val mutStructName = TopLevelCitizenDeclarationNameA("MutStruct", CodeLocationS(0, 0))
    entries = entries ++ Map(mutStructName ->
      StructEnvEntry(StructA(mutStructName, MutableP, Some(MutableP), KindTemplataType, Set(), List(), Set(), Map(), List(), List())))
    val mutInterfaceName = TopLevelCitizenDeclarationNameA("MutInterface", CodeLocationS(0, 0))
    entries = entries ++ Map(mutInterfaceName ->
      InterfaceEnvEntry(InterfaceA(mutInterfaceName, MutableP, Some(MutableP), KindTemplataType, Set(), List(), Set(), Map(), List(), List())))
    val mutStructBorrowName = TopLevelCitizenDeclarationNameA("MutStructBorrow", CodeLocationS(0, 0))
    entries = entries ++ Map(mutStructBorrowName ->
      TemplataEnvEntry(CoordTemplata(Coord(Borrow, StructRef2(FullName2(List(), CitizenName2("MutStruct", List())))))))
    val mutArraySequenceOf4IntName = TopLevelCitizenDeclarationNameA("MutArraySequenceOf4Int", CodeLocationS(0, 0))
    entries = entries ++ Map(mutArraySequenceOf4IntName ->
      TemplataEnvEntry(KindTemplata(ArraySequenceT2(4, RawArrayT2(Coord(Share, Int2()), Mutable)))))
    val voidName = TopLevelCitizenDeclarationNameA("Void", CodeLocationS(0, 0))
    entries = entries ++ Map(voidName -> TemplataEnvEntry(KindTemplata(Void2())))
    val intName = TopLevelCitizenDeclarationNameA("Int", CodeLocationS(0, 0))
    entries = entries ++ Map(intName -> TemplataEnvEntry(KindTemplata(Int2())))
    SimpleEnvironment(entries)
  }

  // Makes an evaluator with some canned data
  def makeCannedEvaluator(): InfererEvaluator[SimpleEnvironment, FakeState] = {
    val templataTemplarDelegate =
      new FakeTemplataTemplarInnerDelegate() {
        override def getMutability(state: FakeState, kind: Kind): Mutability = {
          kind match {
            case StructRef2(FullName2(_, CitizenName2(humanName, _))) if humanName.startsWith("Mut") => Mutable
            case StructRef2(FullName2(_, CitizenName2(humanName, _))) if humanName.startsWith("Imm") => Immutable
            case InterfaceRef2(FullName2(_, CitizenName2(humanName, _))) if humanName.startsWith("Mut") => Mutable
            case InterfaceRef2(FullName2(_, CitizenName2(humanName, _))) if humanName.startsWith("Imm") => Immutable
            case Int2() => Immutable
            case ArraySequenceT2(_, RawArrayT2(_, mutability)) => mutability
            case UnknownSizeArrayT2(RawArrayT2(_, mutability)) => mutability
            case Void2() => Immutable
            case _ => vfail()
          }
        }
        override def evaluateInterfaceTemplata(state: FakeState, templata: InterfaceTemplata, templateArgs: List[ITemplata]): (Kind) = {
          (templata, templateArgs) match {
            case (InterfaceTemplata(_,interfaceName(TopLevelCitizenDeclarationNameA("MutTInterface", _))), List(CoordTemplata(Coord(Share, Int2())) )) => {
              InterfaceRef2(FullName2(List(), CitizenName2("MutTInterface", List(CoordTemplata(Coord(Share, Int2()))))))
            }
            case (InterfaceTemplata(_,interfaceName(TopLevelCitizenDeclarationNameA("MutInterface", _))), List()) => {
              InterfaceRef2(FullName2(List(), CitizenName2("MutInterface", List())))
            }
            case (InterfaceTemplata(_,interfaceName(TopLevelCitizenDeclarationNameA("ImmInterface", _))), List()) => {
              InterfaceRef2(FullName2(List(), CitizenName2("ImmInterface", List())))
            }
          }
        }

        override def evaluateStructTemplata(state: FakeState, templata: StructTemplata, templateArgs: List[ITemplata]): (Kind) = {
          (templata, templateArgs) match {
            case (StructTemplata(_,structName(TopLevelCitizenDeclarationNameA("MutTStruct", _))), List(CoordTemplata(Coord(Share, Int2())) )) => {
              StructRef2(FullName2(List(), CitizenName2("MutTStruct", List(CoordTemplata(Coord(Share, Int2()))))))
            }
            case (StructTemplata(_,structName(TopLevelCitizenDeclarationNameA("MutStruct", _))), List()) => {
              StructRef2(FullName2(List(), CitizenName2("MutStruct", List())))
            }
          }
        }
        override def getInterfaceTemplataType(it: InterfaceTemplata): TemplateTemplataType = {
          it match {
            case InterfaceTemplata(_,interfaceName(TopLevelCitizenDeclarationNameA("MutTInterface", _))) => {
              TemplateTemplataType(List(CoordTemplataType), KindTemplataType)
            }
            case InterfaceTemplata(_, interfaceName(TopLevelCitizenDeclarationNameA("MutInterface", _))) => vfail()
          }
        }
        override def getStructTemplataType(it: StructTemplata): TemplateTemplataType = {
          it match {
            case StructTemplata(_,structName(TopLevelCitizenDeclarationNameA("MutTStruct", _))) => TemplateTemplataType(List(CoordTemplataType), KindTemplataType)
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
            case StructRef2(FullName2(List(), CitizenName2("MutTStruct",List(CoordTemplata(Coord(Share, Int2())))))) => Set(InterfaceRef2(FullName2(List(), CitizenName2("MutTInterface", List(CoordTemplata(Coord(Share, Int2())))))))
            case StructRef2(FullName2(List(), CitizenName2("MutStruct",List()))) => Set(InterfaceRef2(FullName2(List(), CitizenName2("MutInterface", List()))))
            case InterfaceRef2(FullName2(List(), CitizenName2("MutInterface",List()))) => Set()
            case StructRef2(FullName2(List(), CitizenName2("MutSoloStruct",List()))) => Set()
            case _ => vfail(descendantCitizenRef.toString)
          }
        }

        override def citizenIsFromTemplate(state: FakeState, citizen: CitizenRef2, template: ITemplata): (Boolean) = {
          (citizen, template) match {
            case (InterfaceRef2(FullName2(List(), CitizenName2("MutTInterface",List(CoordTemplata(Coord(Share,Int2())))))), InterfaceTemplata(_, interfaceName(TopLevelCitizenDeclarationNameA("MutTInterface", _)))) => true
            case (StructRef2(FullName2(List(), CitizenName2("MutTStruct",List(CoordTemplata(Coord(Share,Int2())))))), StructTemplata(_, structName(TopLevelCitizenDeclarationNameA("MutTStruct", _)))) => true
            case (StructRef2(FullName2(List(), CitizenName2("MutTStruct",List(CoordTemplata(Coord(Share,Int2())))))), InterfaceTemplata(_, interfaceName(TopLevelCitizenDeclarationNameA("MutTInterface", _)))) => false
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
            TemplexTR(RuneTT(CodeRune2("__C"), CoordTemplataType)),
            EqualsTR(
              TemplexTR(RuneTT(CodeRune2("__C"), CoordTemplataType)),
              TemplexTR(OwnershippedTT(BorrowP,NameTT(CodeTypeNameA("ImmInterface"), CoordTemplataType))))),
          Map(CodeRune2("__C") -> CoordTemplataType),
          Set(CodeRune2("__C")),
          Map(),
          List(),
          None,
          true)

    vassert(
      inferences.templatasByRune(CodeRune2("__C")) ==
        CoordTemplata(Coord(Share, InterfaceRef2(FullName2(List(), CitizenName2("ImmInterface", List()))))))
  }

  test("Can infer coord rune from an incoming kind") {
    val (isf @ InferSolveFailure(_, _, _,_,_, _)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(TemplexTR(RuneTT(CodeRune2("C"), CoordTemplataType))),
          Map(CodeRune2("C") -> CoordTemplataType),
          Set(CodeRune2("C")),
          Map(CodeRune2("C") -> KindTemplata(InterfaceRef2(FullName2(List(), CitizenName2("ImmInterface",List(KindTemplata(Int2()))))))),
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
          List(EqualsTR(TemplexTR(RuneTT(CodeRune2("C"), CoordTemplataType)), TemplexTR(RuneTT(CodeRune2("A"), KindTemplataType)))),
          Map(CodeRune2("A") -> KindTemplataType),
          Set(CodeRune2("A")),
          Map(CodeRune2("A") -> KindTemplata(Int2())),
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
            EqualsTR(
              TemplexTR(RuneTT(CodeRune2("C"), CoordTemplataType)),
              CallTR("toRef", List(TemplexTR(RuneTT(CodeRune2("A"), KindTemplataType))), CoordTemplataType))),
          Map(CodeRune2("C") -> CoordTemplataType, CodeRune2("A") -> KindTemplataType),
          Set(CodeRune2("C"), CodeRune2("A")),
          Map(CodeRune2("A") -> KindTemplata(Int2())),
          List(),
          None,
          true)

    conclusions.templatasByRune(CodeRune2("C")) shouldEqual CoordTemplata(Coord(Share, Int2()))
  }

  test("Can explicitly coerce from kind to coord 2") {
    val (InferSolveSuccess(conclusions)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            TemplexTR(RuneTT(ImplicitRune2(0), CoordTemplataType)),
            EqualsTR(TemplexTR(RuneTT(ImplicitRune2(0), CoordTemplataType)),TemplexTR(NameTT(CodeTypeNameA("Int"), CoordTemplataType)))),
          Map(ImplicitRune2(0) -> CoordTemplataType),
          Set(ImplicitRune2(0)),
          Map(),
          List(),
          None,
          true)

    conclusions.templatasByRune(ImplicitRune2(0)) shouldEqual CoordTemplata(Coord(Share, Int2()))
  }

  test("Can match KindTemplataType against StructEnvEntry / StructTemplata") {
    val (InferSolveSuccess(conclusions)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            EqualsTR(
              TemplexTR(RuneTT(CodeRune2("__RetRune"), CoordTemplataType)),
              CallTR("toRef",List(TemplexTR(NameTT(CodeTypeNameA("MutStruct"), KindTemplataType))), TemplateTemplataType(List(KindTemplataType), CoordTemplataType)))),
          Map(CodeRune2("__RetRune") -> CoordTemplataType),
          Set(CodeRune2("__RetRune")),
          Map(),
          List(),
          None,
          true)

    conclusions.templatasByRune(CodeRune2("__RetRune")) shouldEqual
      CoordTemplata(Coord(Own, StructRef2(FullName2(List(), CitizenName2("MutStruct", List())))))
  }

  test("Can infer from simple rules") {
    val (InferSolveSuccess(inferences)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            TemplexTR(RuneTT(ImplicitRune2(0), CoordTemplataType)),
            EqualsTR(TemplexTR(RuneTT(ImplicitRune2(0), CoordTemplataType)),CallTR("toRef", List(TemplexTR(NameTT(CodeTypeNameA("Int"), KindTemplataType))), CoordTemplataType))),
          Map(ImplicitRune2(0) -> CoordTemplataType),
          Set(ImplicitRune2(0)),
          Map(),
          List(),
          None,
          true)

    vassert(inferences.templatasByRune(ImplicitRune2(0)) == CoordTemplata(Coord(Share, Int2())))
  }

  test("Can infer templata from CallAT") {
    val (InferSolveSuccess(inferences)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            EqualsTR(
              TemplexTR(RuneTT(CodeRune2("X"), KindTemplataType)),
              TemplexTR(CallTT(NameTT(CodeTypeNameA("MutTInterface"), TemplateTemplataType(List(CoordTemplataType), KindTemplataType)),List(RuneTT(CodeRune2("T"), CoordTemplataType)), KindTemplataType)))),
          Map(CodeRune2("X") -> KindTemplataType, CodeRune2("T") -> CoordTemplataType),
          Set(CodeRune2("X"), CodeRune2("T")),
          Map(CodeRune2("X") -> KindTemplata(InterfaceRef2(FullName2(List(), CitizenName2("MutTInterface",List(CoordTemplata(Coord(Share, Int2())))))))),
          List(),
          None,
          true)

    vassert(inferences.templatasByRune(CodeRune2("T")) == CoordTemplata(Coord(Share, Int2())))
  }

  test("Can conjure an owning coord from a borrow coord") {
    val (InferSolveSuccess(inferences)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            TemplexTR(RuneTT(CodeRune2("T"), CoordTemplataType)),
            TemplexTR(RuneTT(ImplicitRune2(1337), KindTemplataType)),
            EqualsTR(
              TemplexTR(RuneTT(CodeRune2("T"), CoordTemplataType)),
              ComponentsTR(
                CoordTemplataType,
                List(TemplexTR(OwnershipTT(OwnP)), TemplexTR(RuneTT(ImplicitRune2(1337), KindTemplataType))))),
            TemplexTR(RuneTT(ImplicitRune2(0), CoordTemplataType)),
            EqualsTR(
              TemplexTR(RuneTT(ImplicitRune2(0), CoordTemplataType)),
              ComponentsTR(
                CoordTemplataType,
                List(TemplexTR(OwnershipTT(BorrowP)), TemplexTR(RuneTT(ImplicitRune2(1337), KindTemplataType)))))),
          Map(
            ImplicitRune2(1337) -> KindTemplataType,
            ImplicitRune2(0) -> CoordTemplataType),
          Set(ImplicitRune2(1337), ImplicitRune2(0)),
          Map(),
          List(AtomAP(CaptureA(CodeVarNameA("m"),FinalP),None,ImplicitRuneA(0),None)),
          Some(List(ParamFilter(Coord(Borrow,InterfaceRef2(FullName2(List(), CitizenName2("MutInterface", List())))),None))),
          true)

    vassert(inferences.templatasByRune(CodeRune2("T")) == CoordTemplata(Coord(Own,InterfaceRef2(FullName2(List(), CitizenName2("MutInterface", List()))))))
  }

  test("Rune 0 upcasts to right type, simple") {
    val (InferSolveSuccess(inferences)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            TemplexTR(RuneTT(CodeRune2("__Let0_"), CoordTemplataType)),
            EqualsTR(
              TemplexTR(RuneTT(CodeRune2("__Let0_"), CoordTemplataType)),
              CallTR("toRef", List(TemplexTR(NameTT(CodeTypeNameA("MutInterface"), KindTemplataType))), CoordTemplataType))),
          Map(CodeRune2("__Let0_") -> KindTemplataType),
          Set(CodeRune2("__Let0_")),
          Map(),
          List(AtomAP(CaptureA(CodeVarNameA("x"),FinalP),None,CodeRuneA("__Let0_"),None)),
          Some(List(ParamFilter(Coord(Own,StructRef2(FullName2(List(), CitizenName2("MutStruct",List())))),None))),
          true)

    vassert(inferences.templatasByRune(CodeRune2("__Let0_")) == CoordTemplata(Coord(Own, InterfaceRef2(FullName2(List(), CitizenName2("MutInterface", List()))))))
  }

  test("Rune 0 upcasts to right type templated") {
    val (InferSolveSuccess(inferences)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            TemplexTR(RuneTT(CodeRune2("__Let0_"), CoordTemplataType)),
            EqualsTR(
              TemplexTR(RuneTT(CodeRune2("__Let0_"), CoordTemplataType)),
              CallTR(
                "toRef",
                List(
                  TemplexTR(
                    CallTT(
                      NameTT(CodeTypeNameA("MutTInterface"), TemplateTemplataType(List(CoordTemplataType), KindTemplataType)),
                      List(RuneTT(CodeRune2("T"), CoordTemplataType)),
                      KindTemplataType))),
                CoordTemplataType))),
          Map(CodeRune2("__Let0_") -> KindTemplataType, CodeRune2("T") -> CoordTemplataType),
          Set(CodeRune2("__Let0_"), CodeRune2("T")),
          Map(),
          List(AtomAP(CaptureA(CodeVarNameA("x"),FinalP),None,CodeRuneA("__Let0_"),None)),
          Some(List(ParamFilter(Coord(Own,StructRef2(FullName2(List(), CitizenName2("MutTStruct",List(CoordTemplata(Coord(Share, Int2()))))))),None))),
          true)

    vassert(
      inferences.templatasByRune(CodeRune2("__Let0_")) ==
        CoordTemplata(Coord(Own, InterfaceRef2(FullName2(List(), CitizenName2("MutTInterface", List(CoordTemplata(Coord(Share, Int2())))))))))
    vassert(
      inferences.templatasByRune(CodeRune2("T")) ==
        CoordTemplata(Coord(Share, Int2())))
  }

  test("Tests destructor") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        EqualsTR(
          TemplexTR(RuneTT(CodeRune2("T"), CoordTemplataType)),
          ComponentsTR(
            CoordTemplataType,
            List(
              OrTR(List(TemplexTR(OwnershipTT(OwnP)), TemplexTR(OwnershipTT(ShareP)))),
              CallTR("passThroughIfConcrete",List(TemplexTR(RuneTT(ImplicitRune2(0), KindTemplataType))), KindTemplataType)))),
        EqualsTR(TemplexTR(RuneTT(CodeRune2("V"), CoordTemplataType)),CallTR("toRef",List(TemplexTR(NameTT(CodeTypeNameA("Void"),KindTemplataType))), CoordTemplataType)))
    val atoms =
      List(AtomAP(CaptureA(CodeVarNameA("this"),FinalP),None,CodeRuneA("T"),None))

    val solve =
      (paramFilter: ParamFilter) => {
        makeCannedEvaluator().solve(
          makeCannedEnvironment(),
          FakeState(),
          rules,
          Map(CodeRune2("V") -> CoordTemplataType, CodeRune2("T") -> CoordTemplataType),
          Set(CodeRune2("V"), CodeRune2("T")),
          Map(),
          atoms,
          Some(List(paramFilter)),
          true)
      }

    // Test that it does match a pack
    val packCoord = Coord(Share,PackT2(List(),StructRef2(FullName2(List(), CitizenName2("__Pack",List())))))
    val (InferSolveSuccess(inferencesA)) = solve(ParamFilter(packCoord,None))
    vassert(inferencesA.templatasByRune(CodeRune2("T")) == CoordTemplata(packCoord))

    // Test that it does match a struct
    val structCoord = Coord(Own,StructRef2(FullName2(List(), CitizenName2("MutStruct",List()))))
    val (InferSolveSuccess(inferencesD)) = solve(ParamFilter(structCoord,None))
    vassert(inferencesD.templatasByRune(CodeRune2("T")) == CoordTemplata(structCoord))

    // Test that it doesn't match an int
    val intCoord = Coord(Share,Int2())
    val (isfE @ InferSolveFailure(_, _,_,_, _, _)) = solve(ParamFilter(intCoord,None))
    vassert(isfE.toString.contains("Bad arguments to passThroughIfConcrete"))

    // Test that it doesn't match an interface
    val interfaceCoord = Coord(Own,InterfaceRef2(FullName2(List(), CitizenName2("MutInterface",List()))))
    val (isfF @ InferSolveFailure(_, _, _,_,_, _)) = solve(ParamFilter(interfaceCoord,None))
    vassert(isfF.toString.contains("Bad arguments to passThroughIfConcrete"))
  }

  test("Tests passThroughIfInterface") {
    // Tests that we can make a rule that will only match interfaces.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        EqualsTR(
          TemplexTR(RuneTT(CodeRune2("T"), CoordTemplataType)),
          ComponentsTR(
            CoordTemplataType,
            List(
              OrTR(List(TemplexTR(OwnershipTT(OwnP)), TemplexTR(OwnershipTT(ShareP)))),
              CallTR("passThroughIfInterface",List(TemplexTR(RuneTT(ImplicitRune2(0), KindTemplataType))), KindTemplataType)))),
        EqualsTR(
          TemplexTR(RuneTT(CodeRune2("V"), CoordTemplataType)),
          CallTR("toRef",List(TemplexTR(NameTT(CodeTypeNameA("Void"), KindTemplataType))), CoordTemplataType)))
    val atoms =
      List(AtomAP(CaptureA(CodeVarNameA("this"),FinalP),None,CodeRuneA("T"),None))

    val solve =
      (paramFilter: ParamFilter) => {
        makeCannedEvaluator().solve(
          makeCannedEnvironment(),
          FakeState(),
          rules,
          Map(CodeRune2("T") -> CoordTemplataType, CodeRune2("V") -> CoordTemplataType),
          Set(CodeRune2("T"), CodeRune2("V")),
          Map(),
          atoms,
          Some(List(paramFilter)),
          true)
      }

    // Test that it does match an interface
    val interfaceCoord = Coord(Own,InterfaceRef2(FullName2(List(), CitizenName2("MutInterface",List()))))
    val (InferSolveSuccess(inferencesD)) = solve(ParamFilter(interfaceCoord,None))
    vassert(inferencesD.templatasByRune(CodeRune2("T")) == CoordTemplata(interfaceCoord))

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
        EqualsTR(
          TemplexTR(RuneTT(CodeRune2("T"), CoordTemplataType)),
          ComponentsTR(
            CoordTemplataType,
            List(
              OrTR(List(TemplexTR(OwnershipTT(OwnP)), TemplexTR(OwnershipTT(ShareP)))),
              CallTR("passThroughIfStruct",List(TemplexTR(RuneTT(ImplicitRune2(0), KindTemplataType))), KindTemplataType)))))
    val atoms =
      List(AtomAP(CaptureA(CodeVarNameA("this"), FinalP),None,CodeRuneA("T"),None))

    val solve =
      (paramFilter: ParamFilter) => {
        makeCannedEvaluator().solve(
          makeCannedEnvironment(),
          FakeState(),
          rules,
          Map(CodeRune2("T") -> CoordTemplataType),
          Set(CodeRune2("T")),
          Map(),
          atoms,
          Some(List(paramFilter)),
          true)
      }

    // Test that it does match a struct
    val structCoord = Coord(Own,StructRef2(FullName2(List(), CitizenName2("MutStruct",List()))))
    val (InferSolveSuccess(inferencesD)) = solve(ParamFilter(structCoord,None))
    vassert(inferencesD.templatasByRune(CodeRune2("T")) == CoordTemplata(structCoord))

    // Test that it doesn't match an int
    val intCoord = Coord(Share,Int2())
    val (isfE @ InferSolveFailure(_, _, _,_,_, _)) = solve(ParamFilter(intCoord,None))
    vassert(isfE.toString.contains("Bad arguments to passThroughIfStruct"))

    // Test that it doesn't match an interface
    val interfaceCoord = Coord(Own,InterfaceRef2(FullName2(List(), CitizenName2("MutInterface",List()))))
    val (isfF @ InferSolveFailure(_, _, _,_,_, _)) = solve(ParamFilter(interfaceCoord,None))
    vassert(isfF.toString.contains("Bad arguments to passThroughIfStruct"))

    // Test that it doesn't match an pack
    val packCoord = Coord(Share,PackT2(List(),StructRef2(FullName2(List(), CitizenName2("__Pack",List())))))
    val (isfG @ InferSolveFailure(_, _, _,_,_, _)) = solve(ParamFilter(packCoord,None))
    vassert(isfG.toString.contains("Bad arguments to passThroughIfStruct"))
  }

  test("Test coercing template call result") {
    // Tests that we can make a rule that will only match structs, arrays, packs, sequences.
    // It doesn't have to be in this form, but we do need the capability in some way, so that
    // we can have a templated destructor that matches any of those.

    val rules =
      List(
        TemplexTR(RuneTT(ImplicitRune2(0), CoordTemplataType)),
        EqualsTR(
          TemplexTR(RuneTT(ImplicitRune2(0), CoordTemplataType)),
          TemplexTR(
            CallTT(
              NameTT(CodeTypeNameA("MutTStruct"), TemplateTemplataType(List(CoordTemplataType), KindTemplataType)),
              List(NameTT(CodeTypeNameA("Int"), CoordTemplataType)),
              CoordTemplataType))))
    val atoms =
      List(AtomAP(CaptureA(CodeVarNameA("this"),FinalP),None,CodeRuneA("T"),None))

    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        rules,
        Map(ImplicitRune2(0) -> CoordTemplataType),
        Set(ImplicitRune2(0)),
        Map(),
        atoms,
        None,
        true)

    inferencesD.templatasByRune(ImplicitRune2(0)) shouldEqual
      CoordTemplata(Coord(Own,StructRef2(FullName2(List(), CitizenName2("MutTStruct",List(CoordTemplata(Coord(Share,Int2()))))))))
  }


  test("Test result of a CallAT can coerce to coord") {
    val rules =
      List(
        TemplexTR(RuneTT(CodeRune2("__Par0"), CoordTemplataType)),
        EqualsTR(TemplexTR(RuneTT(CodeRune2("__Par0"), CoordTemplataType)),TemplexTR(NameTT(CodeTypeNameA("MutStruct"), CoordTemplataType))))
    val atoms =
      List(AtomAP(CaptureA(CodeVarNameA("this"),FinalP),None,CodeRuneA("T"),None))

    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        rules,
        Map(CodeRune2("__Par0") -> CoordTemplataType),
        Set(CodeRune2("__Par0")),
        Map(),
        atoms,
        None,
        true)
    inferencesD.templatasByRune(CodeRune2("__Par0")) shouldEqual
      CoordTemplata(Coord(Own,StructRef2(FullName2(List(), CitizenName2("MutStruct",List())))))
  }

  test("Matching a CoordTemplataType onto a CallAT") {
    val rules =
      List(
        TemplexTR(RuneTT(ImplicitRune2(0), CoordTemplataType)),
        EqualsTR(
          TemplexTR(RuneTT(ImplicitRune2(0), CoordTemplataType)),
          TemplexTR(
            CallTT(
              NameTT(CodeTypeNameA("MutTStruct"), TemplateTemplataType(List(CoordTemplataType), KindTemplataType)),
              List(RuneTT(CodeRune2("T"), CoordTemplataType)),
              CoordTemplataType))))

    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        rules,
        Map(ImplicitRune2(0) -> KindTemplataType),
        Set(ImplicitRune2(0)),
        Map(),
        List(AtomAP(CaptureA(CodeVarNameA("x"),FinalP),Some(AbstractAP),ImplicitRuneA(0),None)),
        Some(List(ParamFilter(Coord(Own,StructRef2(FullName2(List(), CitizenName2("MutTStruct",List(CoordTemplata(Coord(Share,Int2()))))))),None))),
        true)
    inferencesD.templatasByRune(ImplicitRune2(0)) shouldEqual
      CoordTemplata(Coord(Own,StructRef2(FullName2(List(), CitizenName2("MutTStruct",List(CoordTemplata(Coord(Share,Int2()))))))))
  }

  test("Test destructuring") {
    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        List(
          TemplexTR(RuneTT(CodeRune2("__Let0_"), CoordTemplataType)),
          TemplexTR(RuneTT(CodeRune2("__Let0__Mem_0"), CoordTemplataType)),
          TemplexTR(RuneTT(CodeRune2("__Let0__Mem_1"), CoordTemplataType))),
        Map(CodeRune2("__Let0_") -> CoordTemplataType, CodeRune2("__Let0__Mem_0") -> CoordTemplataType, CodeRune2("__Let0__Mem_1") -> CoordTemplataType),
        Set(CodeRune2("__Let0_"), CodeRune2("__Let0__Mem_0"), CodeRune2("__Let0__Mem_1")),
        Map(),
        List(
          AtomAP(
            CaptureA(CodeVarNameA("a"), FinalP),
            None,
            CodeRuneA("__Let0_"),
            Some(
              List(
                AtomAP(CaptureA(CodeVarNameA("x"), FinalP),None,CodeRuneA("__Let0__Mem_0"),None),
                AtomAP(CaptureA(CodeVarNameA("y"), FinalP),None,CodeRuneA("__Let0__Mem_1"),None))))),
        Some(List(ParamFilter(Coord(Share,PackT2(List(Coord(Share,Int2()), Coord(Share,Int2())),StructRef2(FullName2(List(), CitizenName2("__Pack",List(CoordTemplata(Coord(Share,Int2())), CoordTemplata(Coord(Share,Int2())))))))),None))),
        true)
    inferencesD.templatasByRune(CodeRune2("__Let0_")) shouldEqual
      CoordTemplata(
        Coord(
          Share,
          PackT2(
            List(Coord(Share,Int2()), Coord(Share,Int2())),
            StructRef2(FullName2(List(), CitizenName2("__Pack",List(CoordTemplata(Coord(Share,Int2())), CoordTemplata(Coord(Share,Int2())))))))))
    inferencesD.templatasByRune(CodeRune2("__Let0__Mem_0")) shouldEqual
      CoordTemplata(Coord(Share,Int2()))
    inferencesD.templatasByRune(CodeRune2("__Let0__Mem_1")) shouldEqual
      CoordTemplata(Coord(Share,Int2()))
  }

  test("Test evaluating array sequence") {
    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        List(
          TemplexTR(RuneTT(ImplicitRune2(0), CoordTemplataType)),
          EqualsTR(
            TemplexTR(RuneTT(ImplicitRune2(0), CoordTemplataType)),
            TemplexTR(RepeaterSequenceTT(MutabilityTT(ImmutableP), IntTT(5),OwnershippedTT(ShareP,NameTT(CodeTypeNameA("Int"), CoordTemplataType)), CoordTemplataType)))),
        Map(ImplicitRune2(0) -> CoordTemplataType),
        Set(ImplicitRune2(0)),
        Map(),
        List(),
        None,
        true)
    inferencesD.templatasByRune(ImplicitRune2(0)) shouldEqual
      CoordTemplata(Coord(Share,ArraySequenceT2(5,RawArrayT2(Coord(Share,Int2()),Immutable))))
  }

  test("Test matching array sequence as coord") {
    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        List(
          EqualsTR(
            TemplexTR(NameTT(CodeTypeNameA("MutArraySequenceOf4Int"), CoordTemplataType)),
            TemplexTR(
              RepeaterSequenceTT(
                RuneTT(CodeRune2("M"), MutabilityTemplataType),
                RuneTT(CodeRune2("N"), IntegerTemplataType),
                RuneTT(CodeRune2("E"), CoordTemplataType),
                CoordTemplataType)))),
        Map(CodeRune2("E") -> CoordTemplataType),
        Set(CodeRune2("E")),
        Map(),
        List(),
        None,
        true)
    inferencesD.templatasByRune(CodeRune2("M")) shouldEqual MutabilityTemplata(Mutable)
    inferencesD.templatasByRune(CodeRune2("N")) shouldEqual IntegerTemplata(4)
    inferencesD.templatasByRune(CodeRune2("E")) shouldEqual CoordTemplata(Coord(Share,Int2()))
  }

  test("Test matching array sequence as kind") {
    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        List(
          EqualsTR(
            TemplexTR(NameTT(CodeTypeNameA("MutArraySequenceOf4Int"), KindTemplataType)),
            TemplexTR(
              RepeaterSequenceTT(
                RuneTT(CodeRune2("M"), MutabilityTemplataType),
                RuneTT(CodeRune2("N"), IntegerTemplataType),
                RuneTT(CodeRune2("E"), CoordTemplataType),
                KindTemplataType)))),
        Map(CodeRune2("E") -> CoordTemplataType),
        Set(CodeRune2("E")),
        Map(),
        List(),
        None,
        true)
    inferencesD.templatasByRune(CodeRune2("M")) shouldEqual MutabilityTemplata(Mutable)
    inferencesD.templatasByRune(CodeRune2("N")) shouldEqual IntegerTemplata(4)
    inferencesD.templatasByRune(CodeRune2("E")) shouldEqual CoordTemplata(Coord(Share,Int2()))
  }

  test("Test array") {
    val (InferSolveSuccess(inferencesD)) =
      makeCannedEvaluator().solve(
        makeCannedEnvironment(),
        FakeState(),
        List(
          EqualsTR(
            TemplexTR(RuneTT(CodeRune2("K"), KindTemplataType)),
            TemplexTR(
              CallTT(
                NameTT(CodeTypeNameA("Array"), TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), KindTemplataType)),
                List(MutabilityTT(MutableP), NameTT(CodeTypeNameA("Int"), CoordTemplataType)),
                KindTemplataType))),
          EqualsTR(
            TemplexTR(RuneTT(CodeRune2("K"), KindTemplataType)),
            TemplexTR(
              CallTT(
                NameTT(CodeTypeNameA("Array"), TemplateTemplataType(List(MutabilityTemplataType, CoordTemplataType), KindTemplataType)),
                List(RuneTT(CodeRune2("M"), MutabilityTemplataType), RuneTT(CodeRune2("T"), CoordTemplataType)),
                KindTemplataType)))),
        Map(CodeRune2("T") -> CoordTemplataType, CodeRune2("M") -> MutabilityTemplataType, CodeRune2("K") -> KindTemplataType),
        Set(CodeRune2("T"), CodeRune2("M"), CodeRune2("K")),
        Map(),
        List(),
        None,
        true)
    inferencesD.templatasByRune(CodeRune2("M")) shouldEqual MutabilityTemplata(Mutable)
    inferencesD.templatasByRune(CodeRune2("T")) shouldEqual CoordTemplata(Coord(Share,Int2()))
  }

  test("Test evaluating isa") {
    val (InferSolveSuccess(_)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            IsaTR(
              TemplexTR(RuneTT(CodeRune2("K"), KindTemplataType)),
              TemplexTR(NameTT(CodeTypeNameA("MutInterface"), KindTemplataType)))),
          Map(CodeRune2("K") -> KindTemplataType),
          Set(CodeRune2("K")),
          Map(CodeRune2("K") -> KindTemplata(StructRef2(FullName2(List(), CitizenName2("MutStruct", List()))))),
          List(),
          None,
          true)

    val (isf @ InferSolveFailure(_, _, _,_,_, _)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            IsaTR(
              TemplexTR(RuneTT(CodeRune2("K"), KindTemplataType)),
              TemplexTR(NameTT(CodeTypeNameA("MutInterface"), KindTemplataType)))),
          Map(CodeRune2("K") -> KindTemplataType),
          Set(CodeRune2("K")),
          Map(CodeRune2("K") -> KindTemplata(StructRef2(FullName2(List(), CitizenName2("MutSoloStruct", List()))))),
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
            EqualsTR(
              TemplexTR(RuneTT(CodeRune2("K"), KindTemplataType)),
              IsaTR(
                TemplexTR(RuneTT(ImplicitRune2(0), KindTemplataType)),
                TemplexTR(NameTT(CodeTypeNameA("MutInterface"), KindTemplataType))))),
          Map(CodeRune2("K") -> KindTemplataType),
          Set(CodeRune2("K")),
          Map(CodeRune2("K") -> KindTemplata(StructRef2(FullName2(List(), CitizenName2("MutStruct", List()))))),
          List(),
          None,
          true)

    val (isf @ InferSolveFailure(_, _,_,_, _, _)) =
      makeCannedEvaluator()
        .solve(
          makeCannedEnvironment(),
          FakeState(),
          List(
            IsaTR(
              TemplexTR(RuneTT(CodeRune2("K"), KindTemplataType)),
              TemplexTR(NameTT(CodeTypeNameA("MutInterface"), KindTemplataType)))),
          Map(CodeRune2("K") -> KindTemplataType),
          Set(CodeRune2("K")),
          Map(CodeRune2("K") -> KindTemplata(StructRef2(FullName2(List(), CitizenName2("MutSoloStruct", List()))))),
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
            EqualsTR(
              TemplexTR(RuneTT(CodeRune2("T"), CoordTemplataType)),
              TemplexTR(OwnershippedTT(targetOwnership, NameTT(CodeTypeNameA(sourceName), CoordTemplataType))))),
          Map(CodeRune2("T") -> CoordTemplataType),
          Set(CodeRune2("T")),
          Map(),
          List(AtomAP(CaptureA(CodeVarNameA("this"),FinalP),None,CodeRuneA("T"),None)),
          None,
          true)
      result
    }

    def expectSuccess(inferSolveResult: IInferSolveResult): Coord = {
      val InferSolveSuccess(inferencesD) = inferSolveResult
      val CoordTemplata(coord) = inferencesD.templatasByRune(CodeRune2("T"))
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
    expectSuccess(run("MutStruct", OwnP)) shouldEqual Coord(Own, StructRef2(FullName2(List(), CitizenName2("MutStruct", List()))))
    expectSuccess(run("MutStruct", BorrowP)) shouldEqual Coord(Borrow, StructRef2(FullName2(List(), CitizenName2("MutStruct", List()))))

    vassert(expectFail(run("MutStructBorrow", ShareP)).contains("Expected a share, but was a borrow"))
    expectSuccess(run("MutStructBorrow", OwnP)) shouldEqual Coord(Own, StructRef2(FullName2(List(), CitizenName2("MutStruct", List()))))
    expectSuccess(run("MutStructBorrow", BorrowP)) shouldEqual Coord(Borrow, StructRef2(FullName2(List(), CitizenName2("MutStruct", List()))))

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
            EqualsTR(
              TemplexTR(NameTT(CodeTypeNameA(sourceName), CoordTemplataType)),
              TemplexTR(OwnershippedTT(targetOwnership, RuneTT(CodeRune2("T"), CoordTemplataType))))),
          Map(CodeRune2("T") -> CoordTemplataType),
          Set(CodeRune2("T")),
          Map(),
          List(),
          None,
          true)
      result
    }

    def expectSuccess(inferSolveResult: IInferSolveResult): Coord = {
      val InferSolveSuccess(inferencesD) = inferSolveResult
      val CoordTemplata(coord) = inferencesD.templatasByRune(CodeRune2("T"))
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
    expectSuccess(run("MutStruct", OwnP)) shouldEqual Coord(Own, StructRef2(FullName2(List(), CitizenName2("MutStruct", List()))))
    // Tries to take the borrow off the incoming own coord... fails.
    vassert(expectFail(run("MutStruct", BorrowP)).contains("Couldn't match incoming Own against expected Borrow"))

    // Tries to take the own off the incoming borrow coord... fails.
    vassert(expectFail(run("MutStructBorrow", OwnP)).contains("Couldn't match incoming Borrow against expected Own"))
    // Takes the borrow off the incoming borrow coord, succeeds and gives us an own.
    expectSuccess(run("MutStructBorrow", BorrowP)) shouldEqual Coord(Own, StructRef2(FullName2(List(), CitizenName2("MutStruct", List()))))
    vassert(expectFail(run("MutStructBorrow", ShareP)).contains("Couldn't match incoming Borrow against expected Share"))
  }
}
