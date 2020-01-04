package net.verdagon.vale

import net.verdagon.vale.vivem.PanicException
import net.verdagon.von.VonInt
import org.scalatest.{FunSuite, Matchers}

class StructTests extends FunSuite with Matchers {
  test("Make struct") {
    val compile = new Compilation(
      """
        |struct Marine { hp *Int; }
        |fn main() {
        |  Marine(9);
        |}
      """.stripMargin)

    compile.run(Vector())
  }

  test("Make struct and get member") {
    val compile = new Compilation(
      """
        |struct Marine { hp *Int; }
        |fn main() {
        |  m = Marine(9);
        |  = m.hp;
        |}
      """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual VonInt(9)
  }

  test("Mutate struct") {
    val compile = new Compilation(
      """
        |struct Marine { hp *Int; }
        |fn main() {
        |  m = Marine(9);
        |  mut m.hp = 4;
        |  = m.hp;
        |}
      """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual VonInt(4)
  }

  test("Destroy members at right times") {
    val compile = new Compilation(
      """
        |struct Weapon { }
        |fn destructor(weapon Weapon) Void {
        |  println("Destroying weapon!");
        |  Weapon() = weapon;
        |}
        |struct Marine {
        |  weapon Weapon;
        |}
        |fn destructor(marine Marine) Void {
        |  println("Destroying marine!");
        |  Marine(weapon) = marine;
        |}
        |fn main() {
        |  Marine(Weapon());
        |}
      """.stripMargin)

    compile.evalForStdout(Vector()) shouldEqual "Destroying marine!\nDestroying weapon!\n"
  }

  test("Mutate destroys member after moving it out of the object") {
    val compile = new Compilation(
      """
        |interface Opt<T> rules(T Ref) { }
        |struct Some<T> rules(T Ref) { value T; }
        |impl Some<T> for Opt<T>;
        |struct None<T> rules(T Ref) { }
        |impl None<T> for Opt<T>;
        |
        |abstract fn getOr<T>(virtual opt &Opt<T>, default T) T;
        |fn getOr<T>(opt &None<T> impl Opt<T>, default T) T {
        |  default
        |}
        |fn getOr<T>(opt &Some<T> impl Opt<T>, default T) T {
        |  opt.value
        |}
        |
        |abstract fn map<T, R>(virtual opt &Opt<T>, func &IFunction1<mut, T, R>) Opt<R>;
        |fn map<T, R>(opt &None<#T> impl Opt<T>, func &IFunction1<mut, T, R>) Opt<R> {
        |  None<R>()
        |}
        |fn map<T, R>(opt &Some<T> impl Opt<T>, func &IFunction1<mut, T, R>) Opt<R> {
        |  Some<R>(func(opt.value))
        |}
        |
        |struct GetMarineWeaponNameFunc { }
        |impl GetMarineWeaponNameFunc for IFunction1<mut, &Marine, Str>;
        |fn __call(this &GetMarineWeaponNameFunc impl IFunction1<mut, &Marine, Str>, m &Marine) Str {
        |  m.weapon.name
        |}
        |
        |struct Weapon {
        |  name Str;
        |  owner Opt<&Marine>;
        |}
        |fn destructor(weapon Weapon) Void {
        |  println("Destroying weapon, owner's weapon is: " + weapon.owner.map(&GetMarineWeaponNameFunc()).getOr("none"));
        |  Weapon(name, owner) = weapon;
        |}
        |struct Marine {
        |  weapon Weapon;
        |}
        |fn destructor(marine Marine) Void {
        |  println("Destroying marine!");
        |  mut marine.weapon.owner = None<&Marine>();
        |  Marine(weapon) = marine;
        |}
        |fn main() {
        |  m = Marine(Weapon("Sword", None<&Marine>()));
        |  mut m.weapon.owner = Some(&m);
        |  mut m.weapon = Weapon("Spear", Some(&m));
        |}
      """.stripMargin)

    // The "Destroying weapon, owner's weapon is: Spear" is the important part.
    // That means that before the weapon's destructor was called, the new weapon
    // was already put in place. This behavior prevents us from accessing a
    // currently-destructing instance from the outside.

    compile.evalForStdout(Vector()) shouldEqual
      """Destroying weapon, owner's weapon is: Spear
        |Destroying marine!
        |Destroying weapon, owner's weapon is: none
        |""".stripMargin
  }


  test("Panic function") {
    val compile = new Compilation(
      """
        |interface Opt<T> rules(T Ref) { }
        |struct Some<T> rules(T Ref) { value T; }
        |impl Some<T> for Opt<T>;
        |struct None<T> rules(T Ref) { }
        |impl None<T> for Opt<T>;
        |
        |abstract fn get<T>(opt virtual &Opt<T>) &T;
        |fn get<T>(opt &None<T> for Opt<T>) &T { panic() }
        |fn get<T>(opt &Some<T> for Opt<T>) &T { opt.value }
        |
        |fn main() {
        |  m Opt<Int> = None<Int>();
        |  = m.get();
        |}
      """.stripMargin)

    try {
      compile.evalForReferend(Vector())
      vfail() // It should panic instead
    } catch {
      case PanicException() =>
    }
  }


  test("Call borrow parameter with shared reference") {
    val compile = new Compilation(
      """fn get(a &T) &T { a }
        |
        |fn main() Int {
        |  = get(6);
        |}
      """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual VonInt(6)
  }
}
