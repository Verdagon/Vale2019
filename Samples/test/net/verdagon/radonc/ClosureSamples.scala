package net.verdagon.radonc

object ClosureSamples {
  val mutate =
    """
      |fn main() Int {
      |  x = 4;
      |  {
      |    mut x = x + 1;
      |  }();
      |  x
      |}
    """.stripMargin
}
