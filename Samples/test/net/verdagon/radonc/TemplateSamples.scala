package net.verdagon.radonc

object TemplateSamples {
  val stampingViaReturn =
    """
      |interface MyInterface:#T rules(#T: Ref) { }
      |
      |struct SomeStruct:#T rules(#T: Ref) { x: #T; }
      |impl SomeStruct:#T for MyInterface:#T;
      |
      |fn doAThing:#T(t: #T) {
      |  SomeStruct:T(t)
      |}
      |
      |fn main() {
      |  doAThing(4);
      |}
    """.stripMargin

  val callingTemplatedConstructor =
    """
      |struct MySome:#T rules(#T: Ref) { value: #T; }
      |fn main() {
      |  MySome:*Int(4).value
      |}
    """.stripMargin
}
