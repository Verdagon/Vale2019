package net.verdagon.vale.midas

import net.verdagon.vale.hammer._
import net.verdagon.vale.scout.{MutabilityP, MutableP}

//object TypeMidas {
//  def translateReferences(miduts0: Miduts, contextMutability: MutabilityP, pointersH: List[ReferenceH]):
//  (Miduts, List[Reference4]) = {
//    pointersH match {
//      case Nil => (miduts0, Nil)
//      case headH :: tailH => {
//        val (miduts1, head4) = translateReference(miduts0, contextMutability, headH)
//        val (miduts2, tail4) = translateReferences(miduts1, contextMutability, tailH)
//        (miduts2, head4 :: tail4)
//      }
//    }
//  }
//
//  def translateReference(miduts0: Miduts, contextMutability: MutabilityP, pointerH: ReferenceH):
//  (Miduts, Reference4) = {
//    pointerH.innerType match {
//      case IntH() => (miduts0, IntReference4())
//      case BoolH() => (miduts0, BoolReference4())
//      case FloatH() => (miduts0, FloatReference4())
//      case ftH @ FunctionTH(paramTypesH, returnTypeH) => {
//        val stackMutability = MutableP // All stack frames are mutable
//        val (miduts1, paramTypes4) = translateReferences(miduts0, stackMutability, paramTypesH)
//        val (miduts2, returnType4) = translateReference(miduts1, stackMutability, returnTypeH)
//        val functionType4 = FunctionT4(ftH, paramTypes4, returnType4)
//        (miduts2, FunctionReference4(functionType4))
//      }
//      case StrH() => vfail("no idea")
//      case sr : StructRefH => {
//        StructMidas.translateStructPointer(miduts0, contextMutability, pointerH)
//      }
//      case ir @ InterfaceRefH(interfaceId, temput) => {
//        StructMidas.translateInterfacePointer(miduts0, contextMutability, pointerH)
//      }
//    }
//  }
//}
