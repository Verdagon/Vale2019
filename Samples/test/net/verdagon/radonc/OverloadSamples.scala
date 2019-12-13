package net.verdagon.radonc

object OverloadSamples {
  val overloads =
    """
      |fn ~(a: Int, b: Int){+(a, b)}
      |fn ~(a: Str, b: Str){+(a, b)}
      |fn main(){3 ~ 3}
    """.stripMargin
}
