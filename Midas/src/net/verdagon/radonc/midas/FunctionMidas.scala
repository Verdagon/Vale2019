package net.verdagon.radonc.midas

import net.verdagon.radonc.hammer._
import net.verdagon.radonc.scout.{ImmutableP, MutableP}
import net.verdagon.radonc.templar.Own

//object FunctionMidas {
//  def translateFunctionRefs(miduts0: Miduts, functionRefs3: List[FunctionRef3]):
//  (Miduts, List[FunctionRef4]) = {
//    functionRefs3 match {
//      case Nil => (miduts0, Nil)
//      case head3 :: tail3 => {
//        val (miduts1, head4) = translateFunctionRef(miduts0, head3)
//        val (miduts2, tail4) = translateFunctionRefs(miduts1, tail3)
//        (miduts2, head4 :: tail4)
//      }
//    }
//  }
//
//  def translateFunctions(miduts0: Miduts, functions3: List[Function3]):
//  (Miduts, List[Function4]) = {
//    functions3 match {
//      case Nil => (miduts0, Nil)
//      case head3 :: tail3 => {
//        val (miduts1, head4) = translateFunction(miduts0, head3)
//        val (miduts2, tail4) = translateFunctions(miduts1, tail3)
//        (miduts2, head4 :: tail4)
//      }
//    }
//  }
//
//  def translateFunction(miduts0: Miduts, function3: Function3):
//  (Miduts, Function4) = {
//    val Function3(prototype3, isAbstract, isExtern, nodes3) = function3
//    val (miduts1, prototype4) = translatePrototype(miduts0, prototype3)
//    val (miduts2, _, _, nodes4) = translateNodes(miduts1, Map(), 1001, nodes3.toList)
//
//    val function4 = Function4(function3, prototype4, isAbstract, isExtern, nodes4.toArray)
//    (miduts2, function4)
//  }
//
//  def translateFunctionRef(miduts0: Miduts, functionRef3: FunctionRef3):
//  (Miduts, FunctionRef4) = {
//    val (miduts1, prototype4) = translatePrototype(miduts0, functionRef3.prototype)
//    val functionRef4 = FunctionRef4(functionRef3, prototype4)
//    (miduts1, functionRef4)
//  }
//
//  def translatePrototype(miduts0: Miduts, prototype3: Prototype3):
//  (Miduts, Prototype4) = {
//    val Prototype3(temput, functionId, humanName, paramTypes3, returnType3) = prototype3;
//    val stackMutability = MutableP // All stack frames can be mutated
//    val (miduts1, paramTypes4) =
//      TypeMidas.translateReferences(miduts0, stackMutability, paramTypes3)
//    val (miduts2, returnType4) =
//      TypeMidas.translateReference(miduts1, stackMutability, returnType3)
//    (miduts2, Prototype4(prototype3, humanName, paramTypes4, returnType4))
//  }
//
//  def translateNodes(
//      miduts0: Miduts,
//      lineMap0: Map[Int, Int],
//      nextLine0: Int,
//      nodes3: List[Node3]):
//  (Miduts, Map[Int, Int], Int, List[Node4]) = {
//    nodes3 match {
//      case Nil => (miduts0, lineMap0, nextLine0, Nil)
//      case head3 :: tail3 => {
//        val (miduts1, lineMaSP, nextLine1, head4) =
//          translateNode(miduts0, lineMap0, nextLine0, head3)
//        val (miduts2, lineMap2, nextLine2, tail4) =
//          translateNodes(miduts1, lineMaSP, nextLine1, tail3)
//        (miduts2, lineMap2, nextLine2, head4 ++ tail4)
//      }
//    }
//  }
//
//  def translateNode(
//      miduts0: Miduts,
//      lineMap0: Map[String, Int],
//      nextLine0: String,
//      node3: Node3):
//  (Miduts, Map[String, Int], String, List[Node4]) = {
//    node3 match {
//      case And3(line3, leftLine, rightLine) => {
//        val node4 = And4(nextLine0, lineMap0(leftLine), lineMap0(rightLine))
//        val lineMaSP = lineMap0.updated(line3, nextLine0)
//        (miduts0, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case Argument3(line3, resultType3, argumentIndex) => {
//        val (miduts1, resultType4) = TypeMidas.translateReference(miduts0, contextMutability = MutableP, resultType3)
//        val node4 = Argument4(nextLine0, argumentIndex, resultType4)
//        val lineMaSP = lineMap0.updated(line3, nextLine0)
//        (miduts1, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case Branch3(line3, conditionLine, trueDestinationLine, falseDestinationLine) => {
//        val node4 = Branch4(nextLine0, conditionLine, trueDestinationLine, falseDestinationLine)
//        val lineMaSP = lineMap0.updated(line3, nextLine0)
//        (miduts0, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case Call3(line3, resultType3, functionLine, functionType, argLines, paramTypes3) => {
//        val (miduts1, paramTypes4) = TypeMidas.translateReferences(miduts0, contextMutability = MutableP, paramTypes3)
//        val (miduts2, resultType4) = TypeMidas.translateReference(miduts1, contextMutability = MutableP, resultType3)
//        val node4 = Call4(nextLine0, functionLine, argLines.map(lineMap0), paramTypes4, resultType4)
//        val lineMaSP = lineMap0.updated(line3, nextLine0)
//        (miduts2, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case ClosureStructLookup3(line3, resultType3, sourceLine, sourceLineType, structId3, memberIndex) => {
//        val structId4 = StructMidas.translateStructId(structId3)
//
//        val stackMutability = MutableP // All stack frames can be mutated
//        val (miduts1, resultType4) =
//          TypeMidas.translateReference(miduts0, stackMutability, resultType3)
//
//        // All this isn't really needed, its just used for the assert.
//        val struct3 = miduts1.program3.structs.find(_.getRef == structId3).get
//        val (miduts2, struct4) = StructMidas.translateStruct(miduts1, struct3)
//        val memberType4 =
//          struct4.members(memberIndex).fieldType match {
//            case ReferenceStructFieldType4(reference4) => {
//              vfail("Closure struct lookups expect a variable address field!")
//            }
//            case VariableAddressStructFieldType4(reference4) => reference4
//          }
//        vassert(memberType4 == resultType4)
//
//        val node4 = ClosureStructLookup4(nextLine0, sourceLine, structId4, memberIndex, resultType4)
//        val lineMaSP = lineMap0.updated(line3, nextLine0)
//        (miduts2, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case ConstantBool3(line3, value) => {
//        val node4 = ConstantBool4(nextLine0, value)
//        val lineMaSP = lineMap0.updated(line3, nextLine0)
//        (miduts0, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case ConstantF643(line3, value) => {
//        val node4 = ConstantF644(nextLine0, value)
//        val lineMaSP = lineMap0.updated(line3, nextLine0)
//        (miduts0, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case ConstantI643(line3, value) => {
//        val node4 = ConstantI644(nextLine0, value)
//        val lineMaSP = lineMap0.updated(line3, nextLine0)
//        (miduts0, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case NewMutableStruct3(line3, structId3, sourceLines) => {
//        val stackMutability = MutableP // Stacks can always be mutated
//        val (miduts1, structReference4) =
//          StructMidas.translateStructPointer(miduts0, stackMutability, Reference3(Own, structId3))
//        val node4 =
//          structReference4 match {
//            case isr4 @ InlineStructReference4(id, ownership, mutability, size, align) => {
//              NewRegisterStruct4(nextLine0, isr4, sourceLines.map(lineMap0))
//            }
//            case fsr4 @ FarStructReference4(id, ownership, mutability) => {
//              NewHeapStruct4(nextLine0, fsr4, sourceLines.map(lineMap0))
//            }
//          }
//        val lineMaSP = lineMap0.updated(line3, nextLine0)
//        (miduts1, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case DestroyStruct3(line3, sourceLine, structId3, sourceType3) => {
//        val stackMutability = MutableP // Stacks can always be mutated
//        val (miduts1, structReference4) =
//          StructMidas.translateStructPointer(miduts0, stackMutability, sourceType3)
//        structReference4 match {
//          case isr4 @ InlineStructReference4(id, ownership, mutability, size, align) => {
//            // Do nothing, it's in a register
//            (miduts1, lineMap0, nextLine0, List())
//          }
//          case fsr4 @ FarStructReference4(id, ownership, mutability) => {
//            // Give that sucker some INDISCRIMINATE JUSTICE
//            val node4 = Free4(nextLine0, lineMap0(sourceLine))
//            val lineMaSP = lineMap0.updated(line3, nextLine0)
//            (miduts1, lineMaSP, nextLine0 + 1, List(node4))
//          }
//        }
//      }
//      case InterfaceToInterfaceUpcast3(line3, sourceInterfaceRef, targetInterfaceRef, resultType3, sourceLine) => {
//        vfail("not yet")
//      }
//      case Jump3(line3, destinationLine) => {
//        vfail("not yet")
//      }
//      case Label3(line) => {
//        vfail("not yet")
//      }
//      case LoadExternFunction3(line3, functionRef3) => {
//        vfail("not yet")
//      }
//      case LoadFunction3(line3, functionRef3) => {
//        vfail("not yet")
//      }
//      case LocalLookup3(line3, varType3, sourceLine, name) => {
//        vfail("not yet")
//      }
//      case Or3(line3, leftLine, rightLine) => {
//        vfail("not yet")
//      }
//      case Panic3(line) => {
//        vfail("not yet")
//      }
//      case Phi3(line3, resultType3, cases) => {
//        vfail("not yet")
//      }
//      case Return3(line3, sourceLine, returnType3) => {
//        vfail("not yet")
//      }
//      case SoftLoad3(line3, resultType3, sourceLine, sourceLineType) => {
//        vfail("not yet")
//      }
//      case Stackify3(line3, sourceLine, varType3, name) => {
//        vfail("not yet")
//      }
//      case Store3(line3, type3, destinationLine, valueLine) => {
//        vfail("not yet")
//      }
//      case StructLookup3(line3, sourceLine, sourceLineType, structType3, memberIndex, resultType3) => {
//        vfail("not yet")
//      }
//      case StructToInterfaceUpcast3(line3, sourceStructRef, targetInterfaceRef, resultType3, sourceLine, sourceLineType) => {
//        vfail("not yet")
//      }
//    }
//  }
//}
