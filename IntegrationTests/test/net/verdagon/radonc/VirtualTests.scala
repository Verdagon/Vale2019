package net.verdagon.radonc

import net.verdagon.radonc.templar.simpleName
import net.verdagon.radonc.templar.templata.{Abstract2, Signature2}
import net.verdagon.radonc.templar.types._
import org.scalatest.{FunSuite, Matchers}

class VirtualTests extends FunSuite with Matchers {

    test("Simple program containing a virtual function") {
      val compile = new Compilation(
        """
          |interface I {}
          |fn doThing(i: virtual I) {4}
          |fn main() {3}
        """.stripMargin)
      val temputs = compile.getTemputs()

      vassert(temputs.getAllUserFunctions.size == 2)
      vassert(temputs.lookupFunction("main").header.returnType == Coord(Share, Int2()))

      val doThing = temputs.lookupFunction(Signature2(simpleName("doThing"), List(Coord(Own, InterfaceRef2(simpleName("I")))))).get
      vassert(doThing.header.params(0).virtuality.get == Abstract2)
    }

  test("Can call virtual function") {
    val compile = new Compilation(
      """
        |interface I {}
        |fn doThing(i: virtual I) {4}
        |fn main(i: I) {
        |  doThing(i)
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()

    vassert(temputs.getAllUserFunctions.size == 2)
    vassert(temputs.lookupFunction("main").header.returnType == Coord(Share, Int2()))

    val doThing = temputs.lookupFunction(Signature2(simpleName("doThing"), List(Coord(Own, InterfaceRef2(simpleName("I")))))).get
    vassert(doThing.header.params(0).virtuality.get == Abstract2)
  }

  test("Can call interface env's function from outside") {
    val compile = new Compilation(
      """
        |interface I {
        |  fn doThing(i: virtual I) Int;
        |}
        |fn main(i: I) {
        |  doThing(i)
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()

    vassert(temputs.getAllUserFunctions.size == 1)
    vassert(temputs.lookupFunction("main").header.returnType == Coord(Share, Int2()))

    val doThing = temputs.lookupFunction(Signature2(simpleName("doThing"), List(Coord(Own, InterfaceRef2(simpleName("I")))))).get
    vassert(doThing.header.params(0).virtuality.get == Abstract2)
  }

}
