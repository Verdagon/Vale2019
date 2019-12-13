package net.verdagon.radonc

import net.verdagon.radonc.templar.env.{AddressibleLocalVariable2, VariableId2}
import net.verdagon.radonc.templar.types.Varying
import net.verdagon.von.VonInt
import org.scalatest.{FunSuite, Matchers}

class ArrayListTest extends FunSuite with Matchers {
  test("Simple ArrayList, no optionals") {
    val compile = new Compilation(
      """
        |struct List<#E> rules(#E: Ref) {
        |  array: __Array<mut, #E>;
        |}
        |fn len<#E>(list: &List<#E>) { len(list.array) }
        |fn add<#E>(list: &List<#E>, newElement: #E) {
        |  newArray =
        |      __Array<mut, E>(len(list) + 1, {(index)
        |        = if (index == len(list)) {
        |            = newElement;
        |          } else {
        |            a = list.array;
        |            = a.(index);
        |          }
        |      });
        |  mut list.array = newArray;
        |}
        |// todo: make that return a &#E
        |fn get<#E>(list: &List<#E>, index: Int) #E {
        |  a = list.array;
        |  = a.(index);
        |}
        |
        |fn main() {
        |  l =
        |      List<Int>(
        |           __Array<mut, Int>(
        |               0,
        |               {(index)
        |                 index
        |               }));
        |  add(&l, 5);
        |  add(&l, 9);
        |  add(&l, 7);
        |  = l.get(1);
        |}
      """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual VonInt(9)
  }

  test("Array list with optionals") {
    val compile = new Compilation(
      Opt.code +
      OptingArrayList.code +
      """
        |
        |fn main() {
        |  l =
        |      List<Int>(
        |          __Array<mut, Opt<Int>>(
        |              0,
        |              {(index)
        |                result: Opt<Int> = Some(index);
        |                = result;
        |              }),
        |          0);
        |  add(&l, 5);
        |  add(&l, 9);
        |  add(&l, 7);
        |  = l.get(1).get();
        |}
      """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual VonInt(9)
  }

  test("Array list zero-constructor") {
    val compile = new Compilation(
      Opt.code +
        OptingArrayList.code +
        """
          |
          |fn main() {
          |  l = List<Int>();
          |  add(&l, 5);
          |  add(&l, 9);
          |  add(&l, 7);
          |  = l.get(1).get();
          |}
        """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual VonInt(9)
  }

  test("Array list len") {
    val compile = new Compilation(
      Opt.code +
        OptingArrayList.code +
        """
          |
          |fn main() {
          |  l = List<Int>();
          |  add(&l, 5);
          |  add(&l, 9);
          |  add(&l, 7);
          |  = l.len();
          |}
        """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual VonInt(3)
  }

  test("Array list set") {
    val compile = new Compilation(
      Opt.code +
        OptingArrayList.code +
        """
          |
          |fn main() {
          |  l = List<Int>();
          |  add(&l, 5);
          |  add(&l, 9);
          |  add(&l, 7);
          |  set(&l, 1, 11);
          |  = l.get(1).get();
          |}
        """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual VonInt(11)
  }

  test("Array list with optionals with mutable element") {
    val compile = new Compilation(
      Opt.code +
      OptingArrayList.code +
        """
          |struct Marine { hp: Int; }
          |
          |fn main() {
          |  l =
          |      List<Marine>(
          |          __Array<mut, Opt<Marine>>(
          |              0,
          |              {(index)
          |                result: Opt<Marine> = Some(Marine(index));
          |                = result;
          |              }),
          |          0);
          |  add(&l, Marine(5));
          |  add(&l, Marine(9));
          |  add(&l, Marine(7));
          |  = l.get(1).get().hp;
          |}
        """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual VonInt(9)
  }

  test("Mutate mutable from in lambda") {
    val compile = new Compilation(
        """
          |struct Marine { hp: Int; }
          |
          |fn main() {
          |  m = Marine(6);
          |  lam = {
          |    mut m = Marine(9);
          |  };
          |  lam();
          |  lam();
          |  = m.hp;
          |}
        """.stripMargin)

    val temputs = compile.getTemputs()
    val main = temputs.lookupFunction("main");
    main.variables.collect({ case AddressibleLocalVariable2(VariableId2(_, "m"), Varying, _) => })

    compile.evalForReferend(Vector()) shouldEqual VonInt(9)
  }

  test("Move mutable from in lambda") {
    val compile = new Compilation(
      Opt.code +
      """
        |struct Marine { hp: Int; }
        |
        |fn main() {
        |  m: Opt<Marine> = Some(Marine(6));
        |  lam = {
        |    m2 = (mut m = None<Marine>())^.get();
        |    = m2.hp;
        |  };
        |  = lam();
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()
    val main = temputs.lookupFunction("main");
    main.variables.collect({ case AddressibleLocalVariable2(VariableId2(_, "m"), Varying, _) => })

    compile.evalForReferend(Vector()) shouldEqual VonInt(6)
  }
}
