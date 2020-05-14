package net.verdagon.vale

import net.verdagon.vale.hammer._
import net.verdagon.vale.metal.{IntH, ReferenceH}
import net.verdagon.vale.{metal => m}
import net.verdagon.vale.templar.types.Share
import org.scalatest.{FunSuite, Matchers}

class HammerTests extends FunSuite with Matchers {
  // Hammer tests only test the general structure of things, not the generated nodes.
  // The generated nodes will be tested by end-to-end tests.

  test("Simple main") {
    val compile = new Compilation(
      "fn main(){3}")
    val hamuts = compile.getHamuts()

    vassert(hamuts.getAllUserFunctions.size == 1)
    vassert(hamuts.getAllUserFunctions.head.prototype.fullName.parts.last == null/*"main"*/);
  }

//  // Make sure a ListNode struct made it out
//  test("Templated struct makes it into hamuts") {
//    val compile = new Compilation(
//      """
//        |struct ListNode<T> imm rules(T: Ref) {
//        |  tail: *ListNode<T>;
//        |}
//        |fn main(a: *ListNode:*Int) {}
//      """.stripMargin)
//    val hamuts = compile.getHamuts()
//    hamuts.structs.find(_.fullName.parts.last.humanName == "ListNode").get;
//  }

  test("Two templated structs make it into hamuts") {
    val compile = new Compilation(
      """
        |interface MyOption<T> imm rules(T Ref) { }
        |struct MyNone<T> imm rules(T Ref) { }
        |impl MyNone<T> for MyOption<T>;
        |struct MySome<T> imm rules(T Ref) { value T; }
        |impl MySome<T> for MyOption<T>;
        |
        |fn main(a *MySome<*Int>, b *MyNone<*Int>) {}
      """.stripMargin)
    val hamuts = compile.getHamuts()
    hamuts.interfaces.find(_.fullName.parts.last == null/*"MyOption"*/).get;

    val mySome = hamuts.structs.find(_.fullName.parts.last == null/*"MySome"*/).get;
    vassert(mySome.members.size == 1);
    vassert(mySome.members.head.tyype == ReferenceH[IntH](m.Share, IntH()))

    val myNone = hamuts.structs.find(_.fullName.parts.last == null/*"MyNone"*/).get;
    vassert(myNone.members.isEmpty);
  }

  test("Virtual and override functions make it into hamuts") {
    val compile = new Compilation(
      """
        |interface Blark imm { }
        |abstract fn wot(b virtual *Blark) *Int;
        |struct MyStruct imm {}
        |impl MyStruct for Blark;
        |fn wot(b *MyStruct for Blark) *Int { 9 }
      """.stripMargin)
    val hamuts = compile.getHamuts()
    hamuts.nonExternFunctions.find(f => f.prototype.fullName.parts.last == null/*"wot"*/).get;
//    hamuts.nonExternFunctions.find(f => f.prototype.humanName == "MyStruct").get;
    vassert(hamuts.abstractFunctions.size == 2)
    vassert(hamuts.getAllUserImplementedFunctions.size == 1)
    vassert(hamuts.getAllUserFunctions.size == 1)
  }

}
