package net.verdagon.radonc

object Assert {
  val code =
    """
      |fn assert(cond: Bool) Void {
      |  assert(cond, "Assertion failed!");
      |}
      |fn assert(cond: Bool, msg: Str) Void {
      |  if {cond == false} {
      |    println(msg);
      |    panic();
      |  }
      |}
      |
      |fn assertEq(a: #T, b: #T) Void {
      |  assert(a == b, "Assertion failed, not equal!");
      |}
      |
      |fn assertEq(a: #T, b: #T, msg: Str) Void {
      |  assert(a == b, msg);
      |}
      |
    """.stripMargin
}
