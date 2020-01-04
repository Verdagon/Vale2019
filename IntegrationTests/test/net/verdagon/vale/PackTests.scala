package net.verdagon.vale

import net.verdagon.vale.templar.PackE2
import net.verdagon.vale.templar.types.{Int2, PackT2}
import net.verdagon.von.VonInt
import org.scalatest.{FunSuite, Matchers}

class PackTests extends FunSuite with Matchers {
  test("Simple pack with one int") {
    val compile = new Compilation(
      """
        |fn main() Int {
        |  (5)
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()
    temputs.lookupFunction("main").header.returnType.referend shouldEqual Int2()
    temputs.all({ case PackE2(_, _, _) => }).size shouldEqual 0

    compile.evalForReferend(Vector()) shouldEqual VonInt(5)
  }

  test("Pack with two ints") {
    val compile = new Compilation(
      """
        |fn main() {
        |  (5, 9)
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()
    temputs.lookupFunction("main").header.returnType.referend.only({ case PackT2(_, _) => })
    temputs.only({ case PackE2(_, _, _) => })
  }

  test("Extract seq") {
    val compile = new Compilation(
      """
        |fn main() {
        |  (x, y, z) = [5, 6, 7];
        |  = x;
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()
    val main = temputs.lookupFunction("main")
    main.all({ case PackE2(List(_, _, _), _, _) => }).size shouldEqual 1

    compile.evalForReferend(Vector()) shouldEqual VonInt(5)
  }

  test("Nested seqs") {
    val compile = new Compilation(
      """
        |fn main() {
        |  (x, (y, z)) = [5, [6, 7]];
        |  = x;
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()
    val main = temputs.lookupFunction("main")
    main .all({ case PackE2(List(_, PackE2(List(_, _), _, _)), _, _) => }).size shouldEqual 1

    compile.evalForReferend(Vector()) shouldEqual VonInt(5)
  }

}
