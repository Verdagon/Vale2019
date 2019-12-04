package net.verdagon.radonc

object TemplatedOption {
  val code =
    """
      |interface MyOption:#T rules(#T: Ref) { }
      |
      |struct MySome:#T rules(#T: Ref) {
      |  value: #T;
      |}
      |impl MySome:#T for MyOption:#T;
      |
      |struct MyNone:#T rules(#T: Ref) { }
      |impl MyNone:#T for MyOption:#T;
      |
      |abstract fn getSize:#T(opt: virtual &MyOption:#T) *Int;
      |fn getSize:#T(opt: &MyNone:#T for MyOption:#T) *Int { 0 }
      |fn getSize:#T(opt: &MySome:#T for MyOption:#T) *Int { 1 }
      |
      |fn main() *Int {
      |  let myOpt: MyOption:Int = MySome:*Int(4);
      |  = getSize(&myOpt);
      |}
    """.stripMargin
}
