package net.verdagon.radonc

object OrdinaryLinkedList {
  val code =
    """
      |interface MyOption imm { }
      |
      |struct MySome imm {
      |  value: *MyList;
      |}
      |impl MySome for MyOption;
      |
      |struct MyNone imm { }
      |impl MyNone for MyOption;
      |
      |
      |struct MyList imm {
      |  value: *Int;
      |  next: *MyOption;
      |}
      |
      |fn printValues(list: *MyList) Void {
      |	 print(list.value);
      |	 printNextValue(list.next);
      |}
      |
      |fn printNextValue(opt: virtual *MyOption) Void { }
      |fn printNextValue(opt: *MyNone for MyOption) Void { }
      |fn printNextValue(opt: *MySome for MyOption) Void {
      |	 printValues(opt.value);
      |}
      |
      |
      |fn main() Int {
      | 	let list = MyList(10, MySome(MyList(20, MySome(MyList(30, MyNone())))));
      | 	printValues(list);
      | 	= 0;
      |}
    """.stripMargin
}
