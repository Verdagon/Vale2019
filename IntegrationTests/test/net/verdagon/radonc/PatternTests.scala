package net.verdagon.radonc

import net.verdagon.radonc.parser.FinalP
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.env.{ReferenceLocalVariable2, VariableId2}
import net.verdagon.radonc.templar.types.{Coord, Final, Int2, Share}
import net.verdagon.von.VonInt
import org.scalatest.{FunSuite, Matchers}

class PatternTests extends FunSuite with Matchers {
  // To get something like this to work would be rather involved.
  //test("Test matching a single-member pack") {
  //  val compile = new Compilation("fn main() { let [x] = (4); = x; }")
  //  val temputs = compile.getTemputs()
  //  val main = temputs.lookupFunction("main")
  //  main.header.returnType shouldEqual Coord(Share, Int2())
  //  compile.evalForReferend(Vector()) shouldEqual VonInt(4)
  //}

  test("Test matching a multiple-member pack of immutables") {
    // Checks that the 5 made it into y, and it was an int
    val compile = new Compilation("fn main() { let [x, y] = (4, 5); = y; }")
    val temputs = compile.getTemputs()
    val main = temputs.lookupFunction("main")
    main.header.returnType shouldEqual Coord(Share, Int2())
    compile.evalForReferend(Vector()) shouldEqual VonInt(5)
  }

  test("Test matching a multiple-member pack of mutables") {
    // Checks that the 5 made it into y, and it was an int
    val compile = new Compilation(
      """
        |struct Marine { hp: *Int; }
        |fn main() { let [x, y] = (Marine(6), Marine(8)); = y.hp; }
      """.stripMargin)
    val temputs = compile.getTemputs()
    val main = temputs.lookupFunction("main");
    main.header.returnType shouldEqual Coord(Share, Int2())
    compile.evalForReferend(Vector()) shouldEqual VonInt(8)
  }

  test("Test matching a multiple-member pack of immutable and own") {
    // Checks that the 5 made it into y, and it was an int
    val compile = new Compilation(
      """
        |struct Marine { hp: *Int; }
        |fn main() { let [x, y] = (7, Marine(8)); = y.hp; }
      """.stripMargin)
    val temputs = compile.getTemputs()
    temputs.functions.head.header.returnType == Coord(Share, Int2())
    compile.evalForReferend(Vector()) shouldEqual VonInt(8)
  }

  test("Test matching a multiple-member pack of immutable and borrow") {
    // Checks that the 5 made it into y, and it was an int
    val compile = new Compilation(
      """
        |struct Marine { hp: *Int; }
        |fn main() {
        |  let m = Marine(8);
        |  let [x, y] = (7, &m);
        |  = y.hp;
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()
    temputs.functions.head.header.returnType == Coord(Share, Int2())
    compile.evalForReferend(Vector()) shouldEqual VonInt(8)
  }
}
