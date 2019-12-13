package net.verdagon.radonc

object OptingArrayList {
val code =
  """struct List<#E> rules(#E: Ref) {
    |  array: __Array<mut, Opt<#E>>;
    |  size: Int;
    |}
    |fn List<#E>() rules(#E: Ref) {
    |  List<E>(__Array<mut, Opt<E>>(0, {(index) panic()}), 0)
    |}
    |fn len<#E>(list: &List<#E>) { list.size }
    |fn add<#E>(list: &List<#E>, newElement: #E) {
    |  if {list.size == list.len()} {
    |    newLen = if {len(list) == 0} { 1 } else { len(list) * 2 };
    |    newArray =
    |        __Array<mut, Opt<E>>(newLen, {(index)
    |          = if {index < len(list)} {
    |              = (mut (list.array.(index)) = None<E>());
    |            } else {
    |              result: Opt<E> = None<E>();
    |              = result;
    |            }
    |        });
    |    mut (list.array) = newArray;
    |  }
    |  mut (list.array.(list.size)) = Some<E>(newElement);
    |  mut (list.size) = list.size + 1;
    |}
    |// todo: make that return a &#E
    |fn get<#E>(list: &List<#E>, index: Int) &Opt<#E> {
    |  a = list.array;
    |  = a.(index);
    |}
    |fn set<#E>(list: &List<#E>, index: Int, value: #E) Void {
    |  mut (list.array.(index)) = Some(value);
    |}
    |fn toArray<#M, #E>(list: &List<#E>) __Array<#M, #E> rules(#M: Mutability) {
    |  __Array<M, E>(list.len(), {(i) list.get(i).get()})
    |}
    |fn toList<#E>(arr: &__Array<_, #E>) List<#E> {
    |  list = List<E>();
    |  arr each {(elem)
    |    list.add(elem);
    |  };
    |  = list;
    |}
    |""".stripMargin
}
