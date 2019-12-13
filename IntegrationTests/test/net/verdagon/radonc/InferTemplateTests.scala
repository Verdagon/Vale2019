package net.verdagon.radonc

import net.verdagon.radonc.templar.simpleName
import net.verdagon.radonc.templar.templata.{CoordTemplata, Parameter2}
import net.verdagon.radonc.templar.types.{Borrow, Coord, Own, StructRef2}
import net.verdagon.von.VonInt
import org.scalatest.{FunSuite, Matchers}

class InferTemplateTests extends FunSuite with Matchers {
  test("Test inferring a borrowed argument") {
    val compile = new Compilation(
      """
        |struct Muta { hp: Int; }
        |fn moo<#T>(m: &#T) { m.hp }
        |fn main() {
        |  x = Muta(10);
        |  = moo(&x);
        |}
      """.stripMargin)

    val moo = compile.getTemputs().lookupFunction("moo")
    moo.header.params match {
      case List(Parameter2("m", _, Coord(Borrow, _))) =>
    }
    moo.header.fullName.steps.last.templateArgs.get shouldEqual List(CoordTemplata(Coord(Own, StructRef2(simpleName("Muta")))))

    compile.evalForReferend(Vector()) shouldEqual VonInt(10)
  }
}
