package net.verdagon.radonc

import net.verdagon.radonc.templar._
import net.verdagon.von.VonStr
import org.scalatest.{FunSuite, Matchers}

class StringTests extends FunSuite with Matchers {
  test("Simple string") {
    val compile = new Compilation(
      """
        |fn main() {
        |  "sprogwoggle"
        |}
      """.stripMargin, false)

    val temputs = compile.getTemputs()
    temputs.only({ case StrLiteral2("sprogwoggle") => })

    compile.evalForReferend(Vector()) shouldEqual VonStr("sprogwoggle")
  }
}
