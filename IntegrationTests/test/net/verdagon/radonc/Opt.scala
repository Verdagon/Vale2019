package net.verdagon.radonc

object Opt {
  val code =
    """interface Opt:#T rules(#T: Ref) { }
      |struct Some:#T rules(#T: Ref) { value: #T; }
      |impl Some:#T for Opt:#T;
      |struct None:#T rules(#T: Ref) { }
      |impl None:#T for Opt:#T;
      |
      |abstract fn empty?:#T(opt: virtual &Opt:#T) Bool;
      |fn empty?:#T(opt: &None:#T for Opt:#T) Bool { true }
      |fn empty?:#T(opt: &Some:#T for Opt:#T) Bool { false }
      |
      |abstract fn get:#T(opt: virtual Opt:#T) #T;
      |fn get:#T(opt: None:#T for Opt:#T) #T { panic() }
      |fn get:#T(opt: Some:#T for Opt:#T) #T {
      |  let :Some:T[value] = opt;
      |  = value;
      |}
      |
      |abstract fn get:#T(opt: virtual &Opt:#T) &#T;
      |fn get:#T(opt: &None:#T for Opt:#T) &#T { panic() }
      |fn get:#T(opt: &Some:#T for Opt:#T) &#T { opt.value }
      |
      |abstract fn getOr:#T(opt: virtual &Opt:#T, default: #T) #T;
      |fn getOr:#T(opt: &None:#T for Opt:#T, default: #T) #T {
      |  default
      |}
      |fn getOr:#T(opt: &Some:#T for Opt:#T, default: #T) #T {
      |  opt.value
      |}
      |
      |abstract fn map:(#T, #R)(opt: virtual &Opt:#T, func: &IFunction1:(mut, &#T, #R)) Opt:#R;
      |fn map:(#T, #R)(opt: &None:#T for Opt:#T, func: &IFunction1:(mut, &#T, #R)) Opt:#R {
      |  None:R()
      |}
      |fn map:(#T, #R)(opt: &Some:#T for Opt:#T, func: &IFunction1:(mut, &#T, #R)) Opt:#R {
      |  Some:R(func(opt.value))
      |}
      |
      |abstract fn getSize:#T(opt: virtual &Opt:#T) *Int;
      |fn getSize:#T(opt: &None:#T for Opt:#T) *Int { 0 }
      |fn getSize:#T(opt: &Some:#T for Opt:#T) *Int { 1 }
      |
    """.stripMargin
}
