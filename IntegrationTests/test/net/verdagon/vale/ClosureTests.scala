package net.verdagon.vale

import net.verdagon.vale.parser.{FinalP, ImmutableP, MutableP, VaryingP}
import net.verdagon.vale.scout.{IEnvironment => _, FunctionEnvironment => _, Environment => _, _}
import net.verdagon.vale.templar._
import net.verdagon.vale.templar.env.{AddressibleLocalVariable2, ReferenceLocalVariable2, VariableId2}
import net.verdagon.vale.templar.templata.{FunctionHeader2, Parameter2}
import net.verdagon.vale.templar.types._
import net.verdagon.von.VonInt
import org.scalatest.{FunSuite, Matchers}

class ClosureTests extends FunSuite with Matchers {

  test("Addressibility") {
    def calc(
        selfBorrowed: IVariableUseCertainty,
        selfMoved: IVariableUseCertainty,
        selfMutated: IVariableUseCertainty,
        childBorrowed: IVariableUseCertainty,
        childMoved: IVariableUseCertainty,
        childMutated: IVariableUseCertainty) = {
      val addressibleIfMutable =
        ExpressionTemplar.determineIfLocalIsAddressible(
          Mutable,
          LocalVariable1(
            "x", VaryingP, selfBorrowed, selfMoved, selfMutated, childBorrowed, childMoved, childMutated))
      val addressibleIfImmutable =
        ExpressionTemplar.determineIfLocalIsAddressible(
          Immutable,
          LocalVariable1(
            "x", VaryingP, selfBorrowed, selfMoved, selfMutated, childBorrowed, childMoved, childMutated))
      (addressibleIfMutable, addressibleIfImmutable)
    }

    // If we don't do anything with the variable, it can be just a reference.
    calc(NotUsed, NotUsed, NotUsed, NotUsed, NotUsed, NotUsed) shouldEqual (false, false)

    // If we or our children only ever read, it can be just a reference.
    calc(Used, NotUsed, NotUsed,      NotUsed, NotUsed, NotUsed) shouldEqual (false, false)
    calc(MaybeUsed, NotUsed, NotUsed, NotUsed, NotUsed, NotUsed) shouldEqual (false, false)
    calc(NotUsed, NotUsed, NotUsed,   Used, NotUsed, NotUsed) shouldEqual (false, false)
    calc(NotUsed, NotUsed, NotUsed,   MaybeUsed, NotUsed, NotUsed) shouldEqual (false, false)

    // If only we mutate it, it can be just a reference.
    calc(NotUsed, NotUsed, Used, NotUsed, NotUsed, NotUsed) shouldEqual (false, false)
    calc(NotUsed, NotUsed, MaybeUsed, NotUsed, NotUsed, NotUsed) shouldEqual (false, false)

    // If we're certain it's moved, it can be just a reference.
    calc(NotUsed, NotUsed, MaybeUsed, NotUsed, NotUsed, NotUsed) shouldEqual (false, false)

    // If we maybe move it, it has to be addressible. Not sure if this is possible
    // though.
    // And if its immutable, doesn't have to be addressible because move = copy.
    calc(NotUsed, MaybeUsed, NotUsed, NotUsed, NotUsed, NotUsed) shouldEqual (true, false)

    // If children might move it, it should be addressible.
    // If its immutable, doesn't have to be addressible because move = copy.
    calc(NotUsed, NotUsed, NotUsed, NotUsed, MaybeUsed, NotUsed) shouldEqual (true, false)

    // Even if we're certain it's moved, it must be addressible.
    // Imagine:
    // fn main() {
    //   m = Marine();
    //   if (something) {
    //     something.consume(m);
    //   } else {
    //     otherthing.consume(m);
    //   }
    // }
    // (or, we can change it so we move it into the closure struct, but that
    // seems weird, i like thinking that closures only ever have borrows or
    // addressibles)
    // However, this doesnt apply to immutable, since move = copy.
    calc(NotUsed, NotUsed, NotUsed, NotUsed, Used, NotUsed) shouldEqual (true, false)

    // If children might mutate it, it has to be addressible.
    calc(NotUsed, NotUsed, NotUsed, NotUsed, NotUsed, MaybeUsed) shouldEqual (true, true)

    // If we're certain children mutate it, it also has to be addressible.
    calc(NotUsed, NotUsed, NotUsed, NotUsed, NotUsed, Used) shouldEqual (true, true)
  }

  test("Captured own is borrow") {
    // Here, the scout determined that the closure is only ever borrowing
    // it (during the dereference to get its member) so templar doesn't put
    // an address into the closure, it instead puts a reference. Specifically,
    // a borrow reference (because why would we want to move this into the
    // closure struct?).
    // This means the closure struct contains a borrow reference. This means
    // the environment in the closure has to match this; the environment has
    // to have a borrow reference instead of an owning reference.

    val compile = new Compilation(
      """
        |struct Marine {
        |  hp *Int;
        |}
        |fn main() {
        |  m = Marine(9);
        |  = { m.hp }();
        |}
      """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual VonInt(9)
  }

  test("Test closure's local variables") {
    val compile = new Compilation("fn main() { x = 4; = {x}(); }")
    val temputs = compile.getTemputs()

    temputs.lookupFunction("main:lam1").variables match {
      case List(
        ReferenceLocalVariable2(VariableId2(_,"__closure"),Final,Coord(Share,StructRef2(simpleName("__Closure:main:lam1")))),
        ReferenceLocalVariable2(VariableId2(_,"__blockresult_0"),Final,Coord(Share,Int2()))
      ) =>
    }
  }

  test("Test returning a nonmutable closured variable from the closure") {
    val compile = new Compilation("fn main() { x = 4; = {x}(); }")
    val temputs = compile.getTemputs()

    // The struct should have an int x in it which is a reference type.
    // It's a reference because we know for sure that it's moved from our child,
    // which means we don't need to check afterwards, which means it doesn't need
    // to be boxed/addressible.
    val closuredVarsStruct = temputs.structs.find(_.fullName.steps.last.humanName == "__Closure:main:lam1").get;
    val expectedMembers = List(StructMember2("x", Final, ReferenceMemberType2(Coord(Share, Int2()))));
    vassert(closuredVarsStruct.members == expectedMembers)

    // Make sure we're doing a referencememberlookup, since it's a reference member
    // in the closure struct.
    temputs.only({
      case ReferenceMemberLookup2(_, "x", _) =>
    })

    // Make sure there's a function that takes in the closured vars struct, and returns an int
    temputs.only({
      case Function2(
      FunctionHeader2(
      simpleName("main:lam1"),
      _,
      _,
      true,
      List(Parameter2(_, None, Coord(Share, StructRef2(simpleName("__Closure:main:lam1"))))),
      Coord(Share, Int2()),
      _),
      _,
      _) =>
    })

    // Make sure we make it with a function pointer and a constructed vars struct
    val main = temputs.functions.find(_.header.fullName.steps.last.humanName == "main").get;
    main.only({
      case Construct2(StructRef2(simpleName("__Closure:main:lam1")), _, _) =>
    })

    // Make sure we call the function somewhere
    main.onlyOf(classOf[FunctionCall2])


    val lambda = temputs.functions.find(_.header.fullName.steps.last.humanName == "main:lam1").get;

    println(lambda.allOf(classOf[LocalLookup2]))
    lambda.only({
      case LocalLookup2(ReferenceLocalVariable2(VariableId2(_, "__closure"), _, _), _) =>
    })

    compile.evalForReferend(Vector()) shouldEqual VonInt(4)
  }

  test("Mutates from inside a closure") {
    val compile = new Compilation(
      """
        |fn main() {
        |  x! = 4;
        |  { mut x = x + 1; }();
        |  = x;
        |}
      """.stripMargin)
    val scoutput = compile.getScoutput()
    val temputs = compile.getTemputs()
    // The struct should have an int x in it.
    val closuredVarsStruct = temputs.structs.find(_.fullName.steps.last.humanName == "__Closure:main:lam1").get;
    val expectedMembers = List(StructMember2("x", Varying, AddressMemberType2(Coord(Share, Int2()))));
    vassert(closuredVarsStruct.members == expectedMembers)

    temputs.only({
      case Mutate2(
        AddressMemberLookup2(_, "x", VariableId2(_, "x"), Coord(Share, Int2())),
        _) =>
    })

    val main = temputs.lookupFunction("main")
    main.only({
      case LetNormal2(AddressibleLocalVariable2(_, Varying, _), _) =>
    })
    main.variables.collect({
      case AddressibleLocalVariable2(_, Varying, _) =>
    }).size shouldEqual 1

    compile.evalForReferend(Vector()) shouldEqual VonInt(5)
  }

  test("Mutates from inside a closure inside a closure") {
    val compile = new Compilation("fn main() { x! = 4; { { mut x = x + 1; }(); }(); = x; }")

    compile.evalForReferend(Vector()) shouldEqual VonInt(5)
  }

}
