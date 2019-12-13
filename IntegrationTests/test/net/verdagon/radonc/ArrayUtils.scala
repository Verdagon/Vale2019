package net.verdagon.radonc

object ArrayUtils {
  val code =
    """
      |fn toArray<#M>(seq: &[:_ #N * #E]) rules(#M: Mutability) {
      |  __Array<M, E>(N, {(i) seq.(i)})
      |}
      |
      |fn each(seq: &[:_ #N * #E], func: #F) Void {
      |  __Array<mut, Int>(N, {(i) func(seq.(i)); = 0; });
      |}
      |
      |//fn map<#M>(seq: &[:_ #N * #E], func: #F) rules(#M: Mutability) {
      |//  __Array<M>(N, {(i) func(seq.(i)) })
      |//}
      |
      |fn each(arr: &__Array<_, #E>, func: #F) Void {
      |  __Array<mut, Int>(arr.len(), {(i) func(arr.(i)); = 0; });
      |}
      |
      |//fn map<#M>(arr: &__Array<_, #E>, func: #F) rules(#M: Mutability) {
      |//  __Array<M>(arr.len(), {(i) func(arr.(i)) })
      |//}
      |
      |fn has(arr: &__Array<_, #E>, elem: #E, equator: #F) Bool {
      |  i = 0;
      |  while (i < arr.len()) {
      |    if ((equator)(arr.(i), elem)) {
      |      ret true;
      |    }
      |    mut i = i + 1;
      |  }
      |  = false;
      |}
      |
      |fn has(arr: &__Array<_, #E>, elem: #E) Bool {
      |  has(arr, elem, ==)
      |}
      |
      |fn has(seq: &[:_ _ * #E], elem: #E, equator: #F) Bool {
      |  i = 0;
      |  while (i < seq.len()) {
      |    if ((equator)(seq.(i), elem)) {
      |      ret true;
      |    }
      |    mut i = i + 1;
      |  }
      |  = false;
      |}
      |
      |fn has(seq: &[:_ _ * #E], elem: #E) Bool {
      |  has(seq, elem, ==)
      |}
      |
      |""".stripMargin
}
