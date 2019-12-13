package net.verdagon.vale

import net.verdagon.vale.templar._
import net.verdagon.vale.templar.templata.{FunctionBanner2, Parameter2}
import net.verdagon.vale.templar.types.{Coord, InterfaceRef2, Own}
import net.verdagon.vale.vivem.{Heap, IntV, StructInstanceV}
import net.verdagon.von.{VonBool, VonFloat, VonInt}
import org.scalatest.{FunSuite, Matchers}

class IntegrationTestsA extends FunSuite with Matchers {

  test("Simple program returning an int") {
    val compile = new Compilation("fn main(){3}")
    compile.evalForReferend(Vector()) shouldEqual VonInt(3)
  }

  test("Hardcoding negative numbers") {
    val compile = new Compilation("fn main(){-3}")
    compile.evalForReferend(Vector()) shouldEqual VonInt(-3)
  }

  test("Taking an argument and returning it") {
    val compile = new Compilation("fn main(a: Int){a}")
    compile.evalForReferend(Vector(IntV(5))) shouldEqual VonInt(5)
  }

  test("Tests adding two numbers") {
    val compile = new Compilation("fn main(){ +(2, 3) }")
    compile.evalForReferend(Vector()) shouldEqual VonInt(5)
  }

  test("Tests adding two floats") {
    val compile = new Compilation("fn main(){ +(2.5, 3.5) }")
    compile.evalForReferend(Vector()) shouldEqual VonFloat(6.0f)
  }

  test("Tests inline adding") {
    val compile = new Compilation("fn main(){ 2 + 3 }")
    compile.evalForReferend(Vector()) shouldEqual VonInt(5)
  }

  test("Tests inline adding more") {
    val compile = new Compilation("fn main(){ 2 + 3 + 4 + 5 + 6 }")
    compile.evalForReferend(Vector()) shouldEqual VonInt(20)
  }

  test("Simple lambda") {
    val compile = new Compilation("fn main(){{7}()}")
    compile.evalForReferend(Vector()) shouldEqual VonInt(7)
  }

  test("Lambda with one magic arg") {
    val compile = new Compilation("fn main(){{_}(3)}")
    compile.evalForReferend(Vector()) shouldEqual VonInt(3)
  }


  // Test that the lambda's arg is the right type, and the name is right
  test("Lambda with a type specified param") {
    val compile = new Compilation("fn main(){{(a: Int) +(a,a)}(3)}");
    compile.evalForReferend(Vector()) shouldEqual VonInt(6)
  }

  test("Test overloads") {
    val compile = new Compilation(
      """
        |fn ~(a: Int, b: Int){+(a, b)}
        |fn ~(a: Str, b: Str){+(a, b)}
        |fn main(){3 ~ 3}
      """.stripMargin)
    compile.evalForReferend(Vector()) shouldEqual VonInt(6)
  }

  test("Test block") {
    val compile = new Compilation("fn main(){true; 200; = 300;}")
    compile.evalForReferend(Vector()) shouldEqual VonInt(300)
  }

  test("Test templates") {
    val compile = new Compilation(
      """
        |fn ~<#T>(a: #T, b: #T) #T { a }
        |fn main(){true ~ false; 2 ~ 2; = 3 ~ 3;}
      """.stripMargin)
    compile.evalForReferend(Vector()) shouldEqual VonInt(3)
  }

  test("Test mutating a local var") {
    val compile = new Compilation("fn main(){a! = 3; mut a = 4; }")
    compile.run(Vector())
  }

  test("Test returning a local mutable var") {
    val compile = new Compilation("fn main(){a! = 3; mut a = 4; = a;}")
    compile.evalForReferend(Vector()) shouldEqual VonInt(4)
  }

  test("Test taking a callable param") {
    val compile = new Compilation(
      """
        |fn do(callable) {callable()}
        |fn main() {do({ 3 })}
      """.stripMargin)
    compile.evalForReferend(Vector()) shouldEqual VonInt(3)
  }

  test("Stamps an interface template via a function parameter") {
    val compile = new Compilation(
      """
        |interface MyInterface<#T> rules(#T: Ref) { }
        |fn doAThing<#T>(i: MyInterface<#T>) { }
        |
        |struct SomeStruct<#T> rules(#T: Ref) { }
        |fn doAThing<#T>(s: SomeStruct<#T>) { }
        |impl SomeStruct<#T> for MyInterface<#T>;
        |
        |fn main(a: SomeStruct<Int>) {
        |  doAThing<Int>(a);
        |}
      """.stripMargin)
    val hamuts = compile.getHamuts()
    val heap = new Heap(System.out)
    val ref =
      heap.add(Own, StructInstanceV(
        hamuts.structs.find(_.fullName.parts.last.humanName == "SomeStruct").get,
        Vector()))
    compile.run(heap, Vector(ref))
  }

  test("Reads a struct member") {
    val compile = new Compilation(
      """
        |struct MyStruct { a: *Int; }
        |fn main() { ms = MyStruct(7); = ms.a; }
      """.stripMargin)
    compile.evalForReferend(Vector()) shouldEqual VonInt(7)
  }

  test("Tests single expression and single statement functions' returns") {
    val compile = new Compilation(
      """
        |struct MyThing { value: *Int; }
        |fn moo() { MyThing(4) }
        |fn main() { moo(); }
      """.stripMargin)
    compile.run(Vector())
  }

  test("Tests calling a templated struct's constructor") {
    val compile = new Compilation(
      """
        |struct MySome<#T> rules(#T: Ref) { value: #T; }
        |fn main() {
        |  MySome<*Int>(4).value
        |}
      """.stripMargin)
    compile.evalForReferend(Vector())
  }

  test("Tests upcasting from a struct to an interface") {
    val compile = new Compilation(
      """
        |interface MyInterface { }
        |struct MyStruct { value: *Int; }
        |impl MyStruct for MyInterface;
        |fn main() {
        |  x: MyInterface = MyStruct(9);
        |}
      """.stripMargin)
    compile.run(Vector())
  }

  test("Tests calling a virtual function") {
    val compile = new Compilation(InheritanceSamples.calling)
    compile.evalForReferend(Vector()) shouldEqual VonInt(7)
  }

  test("Tests making a variable with a pattern") {
    // Tests putting MyOption<Int> as the type of x.
    val compile = new Compilation(
      """
        |interface MyOption<#T> rules(#T: Ref) { }
        |
        |struct MySome<#T> rules(#T: Ref) {}
        |impl MySome<#T> for MyOption<#T>;
        |
        |fn doSomething(opt: MyOption<*Int>) *Int {
        |  = 9;
        |}
        |
        |fn main() Int {
        |	x: MyOption<*Int> = MySome<*Int>();
        |	= doSomething(x);
        |}
      """.stripMargin)
    compile.evalForReferend(Vector()) shouldEqual VonInt(9)
  }

  test("Tests a linked list") {
    val compile = new Compilation(OrdinaryLinkedList.code)
    compile.evalForReferend(Vector())
  }

  test("Tests a templated linked list") {
    val compile = new Compilation(TemplatedLinkedList.code)
    compile.evalForReferend(Vector())
  }

  test("Tests calling an abstract function") {
    val compile = new Compilation(
      """
        |interface MyInterface<#T> rules(#T: Ref) { }
        |abstract fn doThing<#T>(x: virtual MyInterface<#T>) *Int;
        |
        |struct MyStruct<#T> rules(#T: Ref) { }
        |impl MyStruct<#T> for MyInterface<#T>;
        |fn doThing(x: MyStruct<#T> for MyInterface<#T>) *Int {4}
        |
        |fn main() {
        |  x = MyStruct<*Int>();
        |  y = MyStruct<*Str>();
        |  doThing(x);
        |  = doThing(y);
        |}
      """.stripMargin)
    compile.evalForReferend(Vector()) shouldEqual VonInt(4)
  }

  test("Template overrides are stamped") {
    val compile = new Compilation(TemplatedOption.code)
    compile.evalForReferend(Vector()) shouldEqual VonInt(1)
  }

  test("Tests a foreach for a linked list") {
    val compile = new Compilation(ForeachLinkedList.code)
    compile.evalForStdout(Vector()) shouldEqual "102030"
  }

  // When we call a function with a virtual parameter, try stamping for all ancestors in its
  // place.
  // We're stamping all ancestors, and all ancestors have virtual.
  // Virtual starts a function family.
  // So, this checks that it and its three ancestors are all stamped and all get their own
  // function families.
  test("Stamp multiple ancestors") {
    val compile = new Compilation(
      """
        |interface I<#T> imm rules(#T: Ref) { }
        |
        |interface J<#T> imm rules(#T: Ref) { }
        |impl J<#T> for I<#T>;
        |
        |interface K<#T> imm rules(#T: Ref) { }
        |impl K<#T> for J<#T>;
        |
        |struct L<#T> imm rules(#T: Ref) { }
        |impl L<#T> for K<#T>;
        |
        |fn main() {
        |  x = L<*Int>();
        |  = 4;
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()
    compile.evalForReferend(Vector())
  }

  test("Tests recursion") {
    val compile = new Compilation(
      """
        |fn factorial(x: Int) Int {
        |  = if (x == 0) {
        |      1
        |    } else {
        |      x * factorial(x - 1)
        |    }
        |}
        |
        |fn main() {
        |  = factorial(5);
        |}
      """.stripMargin)
    compile.evalForReferend(Vector()) shouldEqual VonInt(120)
  }

  test("Tests floats") {
    val compile = new Compilation(
      """
        |struct Moo imm {
        |  x: Float;
        |}
        |fn main() {
        |  7
        |}
      """.stripMargin)
    compile.evalForReferend(Vector()) shouldEqual VonInt(7)
  }

  test("getOr function") {
    val compile = new Compilation(
      """
        |interface Opt<#T> rules(#T: Ref) { }
        |struct Some<#T> rules(#T: Ref) { value: #T; }
        |impl Some<#T> for Opt<#T>;
        |struct None<#T> rules(#T: Ref) { }
        |impl None<#T> for Opt<#T>;
        |
        |abstract fn getOr<#T>(opt: virtual &Opt<*#T>, default: *#T) *#T;
        |fn getOr<#T>(opt: &None<*#T> for Opt<*#T>, default: *#T) *#T {
        |  default
        |}
        |fn getOr<#T>(opt: &Some<*#T> for Opt<*#T>, default: *#T) *#T {
        |  opt.value
        |}
        |
        |fn main() {
        |  a: Opt<Int> = Some(9);
        |  = a.getOr<Int>(12);
        |}
        |""".stripMargin)

    compile.evalForReferend(Vector()) shouldEqual VonInt(9)
  }

  test("Function return without ret upcasts") {
    val compile = new Compilation(
      """
        |interface Opt { }
        |struct Some { value: Int; }
        |impl Some for Opt;
        |
        |fn doIt() Opt {
        |  Some(9)
        |}
        |
        |fn main() {
        |  a = doIt();
        |  = 3;
        |}
        |""".stripMargin)

    val temputs = compile.getTemputs()
    val doIt = temputs.lookupFunction("doIt")
    doIt.only({
      case StructToInterfaceUpcast2(_, _) =>
    })

    compile.evalForReferend(Vector()) shouldEqual VonInt(3)
  }

  test("Function return with ret upcasts") {
    val compile = new Compilation(
      """
        |interface Opt { }
        |struct Some { value: Int; }
        |impl Some for Opt;
        |
        |fn doIt() Opt {
        |  ret Some(9);
        |}
        |
        |fn main() {
        |  a = doIt();
        |  = 3;
        |}
        |""".stripMargin)

    val temputs = compile.getTemputs()
    val doIt = temputs.lookupFunction("doIt")
    doIt.only({
      case StructToInterfaceUpcast2(_, _) =>
    })

    compile.evalForReferend(Vector()) shouldEqual VonInt(3)
  }

  test("Map function") {
    val compile = new Compilation(
      """
        |interface Opt<#T> rules(#T: Ref) { }
        |struct Some<#T> rules(#T: Ref) { value: #T; }
        |impl Some<#T> for Opt<#T>;
        |struct None<#T> rules(#T: Ref) { }
        |impl None<#T> for Opt<#T>;
        |
        |abstract fn getOr<#T>(opt: virtual &Opt<#T>, default: #T) #T;
        |fn getOr<#T>(opt: &None<#T> for Opt<#T>, default: #T) #T {
        |  default
        |}
        |fn getOr<#T>(opt: &Some<#T> for Opt<#T>, default: #T) #T {
        |  opt.value
        |}
        |
        |abstract fn map<#T, #R>(opt: virtual &Opt<#T>, func: &IFunction1<mut, #T, #R>) Opt<#R>;
        |fn map<#T, #R>(opt: &None<#T> for Opt<#T>, func: &IFunction1<mut, #T, #R>) Opt<#R> {
        |  None<R>()
        |}
        |fn map<#T, #R>(opt: &Some<#T> for Opt<#T>, func: &IFunction1<mut, #T, #R>) Opt<#R> {
        |  Some<R>(func(opt.value))
        |}
        |
        |struct MyEquals9Functor { }
        |impl MyEquals9Functor for IFunction1<mut, Int, Bool>;
        |fn __call(this: &MyEquals9Functor for IFunction1<mut, Int, Bool>, i: Int) Bool { i == 9 }
        |
        |fn main() {
        |  a: Opt<Int> = Some(9);
        |  f = MyEquals9Functor();
        |  b: Opt<Bool> = a.map<Int, Bool>(&f);
        |  = b.getOr<Bool>(false);
        |}
        |""".stripMargin)

    compile.evalForReferend(Vector()) shouldEqual VonBool(true)
  }
}