package net.verdagon.radonc

import org.scalatest.{FunSuite, Matchers}

class PrintTests extends FunSuite with Matchers {
  test("Println'ing an int") {
    val compile = new Compilation(
      """
        |fn main() {
        |  println(6);
        |}
      """.stripMargin)

    compile.evalForStdout(Vector()) shouldEqual "6\n"
  }
}
