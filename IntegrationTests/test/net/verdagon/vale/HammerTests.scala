package net.verdagon.vale

import net.verdagon.vale.hammer._
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
    vassert(hamuts.getAllUserFunctions.head.prototype.fullName.parts.last.humanName == "main");
  }

//  // Make sure a ListNode struct made it out
//  test("Templated struct makes it into hamuts") {
//    val compile = new Compilation(
//      """
//        |struct ListNode<#T> imm rules(#T: Ref) {
//        |  tail: *ListNode<#T>;
//        |}
//        |fn main(a: *ListNode:*Int) {}
//      """.stripMargin)
//    val hamuts = compile.getHamuts()
//    hamuts.structs.find(_.fullName.parts.last.humanName == "ListNode").get;
//  }

  test("Two templated structs make it into hamuts") {
    val compile = new Compilation(
      """
        |interface MyOption<#T> imm rules(#T: Ref) { }
        |struct MyNone<#T> imm rules(#T: Ref) { }
        |impl MyNone<#T> for MyOption<#T>;
        |struct MySome<#T> imm rules(#T: Ref) { value: #T; }
        |impl MySome<#T> for MyOption<#T>;
        |
        |fn main(a: *MySome<*Int>, b: *MyNone<*Int>) {}
      """.stripMargin)
    val hamuts = compile.getHamuts()
    hamuts.interfaces.find(_.fullName.parts.last.humanName == "MyOption").get;

    val mySome = hamuts.structs.find(_.fullName.parts.last.humanName == "MySome").get;
    vassert(mySome.members.size == 1);
    vassert(mySome.members.head.tyype == ReferenceH[IntH](Share, IntH()))

    val myNone = hamuts.structs.find(_.fullName.parts.last.humanName == "MyNone").get;
    vassert(myNone.members.isEmpty);
  }

  test("Generated etables are size 1") {
    val compile = new Compilation(
      """
        |interface MyOption<#T> imm rules(#T: Ref) { }
        |struct MyNone<#T> imm rules(#T: Ref) { }
        |impl MyNone<#T> for MyOption<#T>;
        |struct MySome<#T> imm rules(#T: Ref) { value: #T; }
        |impl MySome<#T> for MyOption<#T>;
        |
        |fn main(a: *MySome<*Int>, b: *MyNone<*Int>) {}
      """.stripMargin)
    val hamuts = compile.getHamuts()
    hamuts.structs
        .filter(s => s.fullName.parts.last.humanName == "MySome" || s.fullName.parts.last.humanName == "MyNone")
        .foreach(struct => {
          vassert(struct.eTable.table.directory.size == 1)
          vassert(struct.eTable.table.combinedBuckets.size == 1)
          vassert(struct.eTable.table.combinedBuckets(0).get._2 == hamuts.interfaces(0).getRef)
        })
  }

  test("Directory for 4 interfaces is still size 1") {
    val compile = new Compilation(
      """
        |interface Blark {}
        |interface Bloop {}
        |interface Blorg {}
        |interface Blerp {}
        |struct MyStruct {}
        |impl MyStruct for Blark;
        |impl MyStruct for Bloop;
        |impl MyStruct for Blorg;
        |impl MyStruct for Blerp;
      """.stripMargin)
    val hamuts = compile.getHamuts()
    val struct = hamuts.structs.find(_.fullName.parts.last.humanName == "MyStruct").get;
    // Our tetris table uses ceil(N/4) directory size, bumped up to the next power of 2. If this doesnt work, perhaps investigate rounding issues...
    vassert(struct.eTable.table.directory.size == 1);

    hamuts.interfaces.foreach(interface => {
      vassert(struct.eTable.table.combinedBuckets.flatMap(_.toList.map(_._2)).contains(interface.getRef))
    })
  }

  test("Virtual and override functions make it into hamuts") {
    val compile = new Compilation(
      """
        |interface Blark imm { }
        |abstract fn wot(b: virtual *Blark) *Int;
        |struct MyStruct imm {}
        |impl MyStruct for Blark;
        |fn wot(b: *MyStruct for Blark) *Int { 9 }
      """.stripMargin)
    val hamuts = compile.getHamuts()
    hamuts.nonExternFunctions.find(f => f.prototype.fullName.parts.last.humanName == "wot").get;
//    hamuts.nonExternFunctions.find(f => f.prototype.humanName == "MyStruct").get;
    vassert(hamuts.abstractFunctions.size == 2)
    vassert(hamuts.getAllUserImplementedFunctions.size == 1)
    vassert(hamuts.getAllUserFunctions.size == 1)
  }

}
