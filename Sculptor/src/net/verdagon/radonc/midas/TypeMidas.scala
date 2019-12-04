package net.verdagon.radonc.midas

//object TypeMidas {
//  def translateReferences(miduts0: Miduts, contextMutability: MutabilityP, pointers3: List[Reference3]):
//  (Miduts, List[Reference4]) = {
//    pointers3 match {
//      case Nil => (miduts0, Nil)
//      case head3 :: tail3 => {
//        val (miduts1, head4) = translateReference(miduts0, contextMutability, head3)
//        val (miduts2, tail4) = translateReferences(miduts1, contextMutability, tail3)
//        (miduts2, head4 :: tail4)
//      }
//    }
//  }
//
//  def translateReference(miduts0: Miduts, contextMutability: MutabilityP, pointer3: Reference3):
//  (Miduts, Reference4) = {
//    pointer3.innerType match {
//      case Int3() => (miduts0, IntReference4())
//      case Bool3() => (miduts0, BoolReference4())
//      case Float3() => (miduts0, FloatReference4())
//      case ft3 @ FunctionT3(paramTypes3, returnType3) => {
//        val stackMutability = MutableP // All stack frames are mutable
//        val (miduts1, paramTypes4) = translateReferences(miduts0, stackMutability, paramTypes3)
//        val (miduts2, returnType4) = translateReference(miduts1, stackMutability, returnType3)
//        val functionType4 = FunctionT4(ft3, paramTypes4, returnType4)
//        (miduts2, FunctionReference4(functionType4))
//      }
//      case Str3() => vfail("no idea")
//      case sr : StructRef3 => {
//        StructMidas.translateStructPointer(miduts0, contextMutability, pointer3)
//      }
//      case ir @ InterfaceRef3(interfaceId, temput) => {
//        StructMidas.translateInterfacePointer(miduts0, contextMutability, pointer3)
//      }
//    }
//  }
//}
