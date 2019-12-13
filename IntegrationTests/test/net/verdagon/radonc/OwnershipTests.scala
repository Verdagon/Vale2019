package net.verdagon.radonc

import net.verdagon.radonc.parser.{CaptureP, FinalP, VaryingP}
import net.verdagon.radonc.scout._
import net.verdagon.radonc.scout.patterns.AtomSP
import net.verdagon.radonc.scout.rules.{CoordTypeSR, TypedSR}
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.env.{ReferenceLocalVariable2, VariableId2}
import net.verdagon.radonc.templar.templata.{FunctionHeader2, Prototype2}
import net.verdagon.radonc.templar.types._
import net.verdagon.von.VonInt
import org.scalatest.{FunSuite, Matchers}

class OwnershipTests extends FunSuite with Matchers {
  test("Borrowing a temporary mutable makes a local var") {
    val compile = new Compilation(
      """
        |struct Muta { hp: *Int; }
        |fn main() {
        |  = (&Muta(9)).hp;
        |}
      """.stripMargin)

    val main = compile.getTemputs().lookupFunction("main")
    main.only({
      case LetAndLend2(ReferenceLocalVariable2(VariableId2(0, "__0_temp"), Final, Coord(Own, StructRef2(simpleName("Muta")))), refExpr) => {
        refExpr.resultRegister.reference match {
          case Coord(Own, StructRef2(simpleName("Muta"))) =>
        }
      }
    })

    compile.evalForReferend(Vector()) shouldEqual VonInt(9)
  }

  test("When statement result is an owning ref, calls destructor") {
    val compile = new Compilation(
      """
        |struct Muta { }
        |
        |fn destructor(m: ^Muta) Void {
        |  println("Destroying!");
        |  :Muta[] = m;
        |}
        |
        |fn main() {
        |  Muta();
        |}
      """.stripMargin)

    val main = compile.getTemputs().lookupFunction("main")
    main.only({ case FunctionLookup2(functionName("destructor")) => })
    main.all({ case FunctionPointerCall2(_, _) => }).size shouldEqual 2

    compile.evalForStdout(Vector()) shouldEqual "Destroying!\n"
  }

  test("Saves return value then destroys temporary") {
    val compile = new Compilation(
      """
        |struct Muta { hp: *Int; }
        |
        |fn destructor(m: ^Muta) {
        |  println("Destroying!");
        |  :Muta[hp] = m;
        |}
        |
        |fn main() {
        |  = (Muta(10)).hp;
        |}
      """.stripMargin)

    val main = compile.getTemputs().lookupFunction("main")
    main.only({ case FunctionLookup2(functionName("destructor")) => })

    compile.evalForReferendAndStdout(Vector()) shouldEqual (VonInt(10), "Destroying!\n")
  }

  test("Calls destructor on local var") {
    val compile = new Compilation(
      """
        |struct Muta { }
        |
        |fn destructor(m: ^Muta) {
        |  println("Destroying!");
        |  :Muta[] = m;
        |}
        |
        |fn main() {
        |  a = Muta();
        |}
      """.stripMargin)

    val main = compile.getTemputs().lookupFunction("main")
    main.only({ case FunctionLookup2(functionName("destructor")) => })
    main.all({ case FunctionPointerCall2(_, _) => }).size shouldEqual 2

    compile.evalForStdout(Vector()) shouldEqual "Destroying!\n"
  }

  test("Calls destructor on local var unless moved") {
    // Should call the destructor in moo, but not in main
    val compile = new Compilation(
      """
        |struct Muta { }
        |
        |fn destructor(m: ^Muta) {
        |  println("Destroying!");
        |  :Muta[] = m;
        |}
        |
        |fn moo(m: ^Muta) {
        |}
        |
        |fn main() {
        |  a = Muta();
        |  moo(a);
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()

    // Destructor should only be calling println, NOT the destructor (itself)
    val destructor = temputs.lookupUserFunction("destructor")
    // The only function lookup should be println
    destructor.only({ case FunctionLookup2(functionName("println")) => })
    // Only one call (the above println)
    destructor.all({ case FunctionPointerCall2(_, _) => }).size shouldEqual 1

    // moo should be calling the destructor
    val moo = temputs.lookupFunction("moo")
    moo.only({ case FunctionLookup2(functionName("destructor")) => })
    moo.only({ case FunctionPointerCall2(_, _) => })

    // main should not be calling the destructor
    val main = temputs.lookupFunction("main")
    main.all({ case FunctionLookup2(functionName("destructor")) => true }).size shouldEqual 0

    compile.evalForStdout(Vector()) shouldEqual "Destroying!\n"
  }

  test("Saves return value then destroys local var") {
    val compile = new Compilation(
      """
        |struct Muta { hp: *Int; }
        |
        |fn destructor(m: ^Muta) {
        |  println("Destroying!");
        |  :Muta[hp] = m;
        |}
        |
        |fn main() {
        |  a = Muta(10);
        |  = a.hp;
        |}
      """.stripMargin)

    val main = compile.getTemputs().lookupFunction("main")
    main.only({ case FunctionLookup2(functionName("destructor")) => })
    main.all({ case FunctionPointerCall2(_, _) => }).size shouldEqual 2

    compile.evalForReferendAndStdout(Vector()) shouldEqual (VonInt(10), "Destroying!\n")
  }

  test("Gets from temporary struct a member's member") {
    val compile = new Compilation(
      """
        |struct Wand {
        |  charges: *Int;
        |}
        |struct Wizard {
        |  wand: ^Wand;
        |}
        |fn main() {
        |  = Wizard(Wand(10)).wand.charges;
        |}
      """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual VonInt(10)
  }

  // test that when we create a block closure, we hoist to the beginning its constructor,
  // and hoist to the end its destructor.

  // test that when we borrow an owning, we hoist its destructor to the end.

  test("Checks that we stored a borrowed temporary in a local") {
    // Checking for 2 because one for the temporary, and one inside the
    // templated wrapper function.
    val compile = new Compilation(
      """
        |struct Muta { }
        |fn main() {
        |  __checkvarrc(&Muta(), 1)
        |}
      """.stripMargin)

    // Should be a temporary for this object
    compile.getTemputs().lookupFunction("main")
            .allOf(classOf[LetAndLend2]).size shouldEqual 1

    compile.run(Vector())
  }

  test("Var RC for one local") {
    val compile = new Compilation(
      """
        |struct Muta { }
        |fn main() {
        |  a = Muta();
        |  __checkvarrc(&a, 1);
        |}
      """.stripMargin)

    val main = compile.getTemputs().lookupFunction("main")
    main.allOf(classOf[LetNormal2]).size shouldEqual 1

    compile.run(Vector())
  }

  test("Unstackifies local vars") {
    val compile = new Compilation(
      """
        |fn main() {
        |  mut i = 0;
        |  = i;
        |}
      """.stripMargin)

    val main = compile.getTemputs().lookupFunction("main")

    val numVariables = main.all({ case ReferenceLocalVariable2(_, _, _) => }).size
    main.all({ case LetAndLend2(_, _) => case LetNormal2(_, _) => }).size shouldEqual numVariables
    main.all({ case Unlet2(_) => }).size shouldEqual numVariables
  }





//
//  test("Moving doesn't affect ref count") {
//    val compile = new Compilation(
//      """
//        |struct Muta { }
//        |fn main() {
//        |  a = Muta();
//        |  b = a;
//        |  = __varrc(&b);
//        |}
//      """.stripMargin)
//
//    compile.evalForReferend(Vector()) shouldEqual VonInt(1)
//  }
//
//  test("Wingman catches hanging borrow") {
//    val compile = new Compilation(
//      """
//        |struct MutaA { }
//        |struct MutaB {
//        |  a: &MutaA;
//        |}
//        |fn main() {
//        |  a = MutaA();
//        |  b = MutaB(&a);
//        |  c = a;
//        |}
//      """.stripMargin)
//
//    try {
//      compile.evalForReferend(Vector())
//      fail();
//    } catch {
//      case VivemPanic("Dangling borrow") =>
//    }
//  }
}
