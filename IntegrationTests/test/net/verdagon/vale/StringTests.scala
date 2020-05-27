package net.verdagon.vale

import net.verdagon.vale.templar._
import net.verdagon.von.VonStr
import org.scalatest.{FunSuite, Matchers}
import net.verdagon.vale.driver.Compilation

class StringTests extends FunSuite with Matchers {
  test("Simple string") {
    val compile = new Compilation(
      """
        |fn main() {
        |  "sprogwoggle"
        |}
      """.stripMargin, false)

    val temputs = compile.getTemputs()
    temputs.lookupFunction("main").only({ case StrLiteral2("sprogwoggle") => })

    compile.evalForReferend(Vector()) shouldEqual VonStr("sprogwoggle")
  }
}
