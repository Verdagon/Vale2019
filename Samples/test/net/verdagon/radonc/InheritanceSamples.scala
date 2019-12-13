package net.verdagon.radonc

object InheritanceSamples {
  val upcasting =
    """
      |interface MyInterface { }
      |struct MyStruct { value: *Int; }
      |impl MyStruct for MyInterface;
      |fn main() {
      |  x: MyInterface = MyStruct(9);
      |}
    """.stripMargin

  val calling =
    """
      |interface Car { }
      |abstract fn doCivicDance(this: virtual Car) Int;
      |
      |struct Civic {}
      |impl Civic for Car;
      |fn doCivicDance(civic: Civic for Car) Int {
      |	= 4;
      |}
      |
      |struct Toyota {}
      |impl Toyota for Car;
      |fn doCivicDance(toyota: Toyota for Car) Int {
      |	= 7;
      |}
      |
      |fn main() Int {
      |	x: Car = Toyota();
      |	= doCivicDance(x);
      |}
    """.stripMargin

  val callingThroughBorrow =
    """
      |interface Car { }
      |abstract fn doCivicDance(this: virtual &Car) Int;
      |
      |struct Civic {}
      |impl Civic for Car;
      |fn doCivicDance(civic: &Civic for Car) Int {
      |	= 4;
      |}
      |
      |struct Toyota {}
      |impl Toyota for Car;
      |fn doCivicDance(toyota: &Toyota for Car) Int {
      |	= 7;
      |}
      |
      |fn main() Int {
      |	x: Car = Toyota();
      | b = &x;
      |	= doCivicDance(b);
      |}
    """.stripMargin

  val callingAbstract =

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
    """.stripMargin
}
