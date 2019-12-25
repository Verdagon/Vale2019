package net.verdagon.vale.midas

//object FunctionMidas {
//  def translateFunctionRefs(miduts0: Miduts, functionRefsH: List[FunctionRefH]):
//  (Miduts, List[FunctionRef4]) = {
//    functionRefsH match {
//      case Nil => (miduts0, Nil)
//      case headH :: tailH => {
//        val (miduts1, head4) = translateFunctionRef(miduts0, headH)
//        val (miduts2, tail4) = translateFunctionRefs(miduts1, tailH)
//        (miduts2, head4 :: tail4)
//      }
//    }
//  }
//
//  def translateFunctions(miduts0: Miduts, functionsH: List[FunctionH]):
//  (Miduts, List[Function4]) = {
//    functionsH match {
//      case Nil => (miduts0, Nil)
//      case headH :: tailH => {
//        val (miduts1, head4) = translateFunction(miduts0, headH)
//        val (miduts2, tail4) = translateFunctions(miduts1, tailH)
//        (miduts2, head4 :: tail4)
//      }
//    }
//  }
//
//  def translateFunction(miduts0: Miduts, functionH: FunctionH):
//  (Miduts, Function4) = {
//    val FunctionH(prototypeH, isAbstract, isExtern, nodesH) = functionH
//    val (miduts1, prototype4) = translatePrototype(miduts0, prototypeH)
//    val (miduts2, _, _, nodes4) = translateNodes(miduts1, Map(), 1001, nodesH.toList)
//
//    val function4 = Function4(functionH, prototype4, isAbstract, isExtern, nodes4.toArray)
//    (miduts2, function4)
//  }
//
//  def translateFunctionRef(miduts0: Miduts, functionRefH: FunctionRefH):
//  (Miduts, FunctionRef4) = {
//    val (miduts1, prototype4) = translatePrototype(miduts0, functionRefH.prototype)
//    val functionRef4 = FunctionRef4(functionRefH, prototype4)
//    (miduts1, functionRef4)
//  }
//
//  def translatePrototype(miduts0: Miduts, prototypeH: PrototypeH):
//  (Miduts, Prototype4) = {
//    val PrototypeH(temput, functionId, humanName, paramTypesH, returnTypeH) = prototypeH;
//    val stackMutability = MutableP // All stack frames can be mutated
//    val (miduts1, paramTypes4) =
//      TypeMidas.translateReferences(miduts0, stackMutability, paramTypesH)
//    val (miduts2, returnType4) =
//      TypeMidas.translateReference(miduts1, stackMutability, returnTypeH)
//    (miduts2, Prototype4(prototypeH, humanName, paramTypes4, returnType4))
//  }
//
//  def translateNodes(
//      miduts0: Miduts,
//      lineMap0: Map[Int, Int],
//      nextLine0: Int,
//      nodesH: List[NodeH]):
//  (Miduts, Map[Int, Int], Int, List[Node4]) = {
//    nodesH match {
//      case Nil => (miduts0, lineMap0, nextLine0, Nil)
//      case headH :: tailH => {
//        val (miduts1, lineMaSP, nextLine1, head4) =
//          translateNode(miduts0, lineMap0, nextLine0, headH)
//        val (miduts2, lineMap2, nextLine2, tail4) =
//          translateNodes(miduts1, lineMaSP, nextLine1, tailH)
//        (miduts2, lineMap2, nextLine2, head4 ++ tail4)
//      }
//    }
//  }
//
//  def translateNode(
//      miduts0: Miduts,
//      lineMap0: Map[String, Int],
//      nextLine0: String,
//      nodeH: NodeH):
//  (Miduts, Map[String, Int], String, List[Node4]) = {
//    nodeH match {
//      case AndH(lineH, leftLine, rightLine) => {
//        val node4 = And4(nextLine0, lineMap0(leftLine), lineMap0(rightLine))
//        val lineMaSP = lineMap0.updated(lineH, nextLine0)
//        (miduts0, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case ArgumentH(lineH, resultTypeH, argumentIndex) => {
//        val (miduts1, resultType4) = TypeMidas.translateReference(miduts0, contextMutability = MutableP, resultTypeH)
//        val node4 = Argument4(nextLine0, argumentIndex, resultType4)
//        val lineMaSP = lineMap0.updated(lineH, nextLine0)
//        (miduts1, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case BranchH(lineH, conditionLine, trueDestinationLine, falseDestinationLine) => {
//        val node4 = Branch4(nextLine0, conditionLine, trueDestinationLine, falseDestinationLine)
//        val lineMaSP = lineMap0.updated(lineH, nextLine0)
//        (miduts0, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case CallH(lineH, resultTypeH, functionLine, functionType, argLines, paramTypesH) => {
//        val (miduts1, paramTypes4) = TypeMidas.translateReferences(miduts0, contextMutability = MutableP, paramTypesH)
//        val (miduts2, resultType4) = TypeMidas.translateReference(miduts1, contextMutability = MutableP, resultTypeH)
//        val node4 = Call4(nextLine0, functionLine, argLines.map(lineMap0), paramTypes4, resultType4)
//        val lineMaSP = lineMap0.updated(lineH, nextLine0)
//        (miduts2, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case ClosureStructLookupH(lineH, resultTypeH, sourceLine, sourceLineType, structIdH, memberIndex) => {
//        val structId4 = StructMidas.translateStructId(structIdH)
//
//        val stackMutability = MutableP // All stack frames can be mutated
//        val (miduts1, resultType4) =
//          TypeMidas.translateReference(miduts0, stackMutability, resultTypeH)
//
//        // All this isn't really needed, its just used for the assert.
//        val structH = miduts1.programH.structs.find(_.getRef == structIdH).get
//        val (miduts2, struct4) = StructMidas.translateStruct(miduts1, structH)
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
//        val lineMaSP = lineMap0.updated(lineH, nextLine0)
//        (miduts2, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case ConstantBoolH(lineH, value) => {
//        val node4 = ConstantBool4(nextLine0, value)
//        val lineMaSP = lineMap0.updated(lineH, nextLine0)
//        (miduts0, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case ConstantF64H(lineH, value) => {
//        val node4 = ConstantF644(nextLine0, value)
//        val lineMaSP = lineMap0.updated(lineH, nextLine0)
//        (miduts0, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case ConstantI64H(lineH, value) => {
//        val node4 = ConstantI644(nextLine0, value)
//        val lineMaSP = lineMap0.updated(lineH, nextLine0)
//        (miduts0, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case NewMutableStructH(lineH, structIdH, sourceLines) => {
//        val stackMutability = MutableP // Stacks can always be mutated
//        val (miduts1, structReference4) =
//          StructMidas.translateStructPointer(miduts0, stackMutability, ReferenceH(m.Own, structIdH))
//        val node4 =
//          structReference4 match {
//            case isr4 @ InlineStructReference4(id, ownership, mutability, size, align) => {
//              NewRegisterStruct4(nextLine0, isr4, sourceLines.map(lineMap0))
//            }
//            case fsr4 @ FarStructReference4(id, ownership, mutability) => {
//              NewHeapStruct4(nextLine0, fsr4, sourceLines.map(lineMap0))
//            }
//          }
//        val lineMaSP = lineMap0.updated(lineH, nextLine0)
//        (miduts1, lineMaSP, nextLine0 + 1, List(node4))
//      }
//      case DestroyStructH(lineH, sourceLine, structIdH, sourceTypeH) => {
//        val stackMutability = MutableP // Stacks can always be mutated
//        val (miduts1, structReference4) =
//          StructMidas.translateStructPointer(miduts0, stackMutability, sourceTypeH)
//        structReference4 match {
//          case isr4 @ InlineStructReference4(id, ownership, mutability, size, align) => {
//            // Do nothing, it's in a register
//            (miduts1, lineMap0, nextLine0, List())
//          }
//          case fsr4 @ FarStructReference4(id, ownership, mutability) => {
//            // Give that sucker some INDISCRIMINATE JUSTICE
//            val node4 = Free4(nextLine0, lineMap0(sourceLine))
//            val lineMaSP = lineMap0.updated(lineH, nextLine0)
//            (miduts1, lineMaSP, nextLine0 + 1, List(node4))
//          }
//        }
//      }
//      case InterfaceToInterfaceUpcastH(lineH, sourceInterfaceRef, targetInterfaceRef, resultTypeH, sourceLine) => {
//        vfail("not yet")
//      }
//      case JumpH(lineH, destinationLine) => {
//        vfail("not yet")
//      }
//      case LabelH(line) => {
//        vfail("not yet")
//      }
//      case LoadExternFunctionH(lineH, functionRefH) => {
//        vfail("not yet")
//      }
//      case LoadFunctionH(lineH, functionRefH) => {
//        vfail("not yet")
//      }
//      case LocalLookupH(lineH, varTypeH, sourceLine, name) => {
//        vfail("not yet")
//      }
//      case OrH(lineH, leftLine, rightLine) => {
//        vfail("not yet")
//      }
//      case PanicH(line) => {
//        vfail("not yet")
//      }
//      case PhiH(lineH, resultTypeH, cases) => {
//        vfail("not yet")
//      }
//      case ReturnH(lineH, sourceLine, returnTypeH) => {
//        vfail("not yet")
//      }
//      case SoftLoadH(lineH, resultTypeH, sourceLine, sourceLineType) => {
//        vfail("not yet")
//      }
//      case StackifyH(lineH, sourceLine, varTypeH, name) => {
//        vfail("not yet")
//      }
//      case StoreH(lineH, typeH, destinationLine, valueLine) => {
//        vfail("not yet")
//      }
//      case StructLookupH(lineH, sourceLine, sourceLineType, structTypeH, memberIndex, resultTypeH) => {
//        vfail("not yet")
//      }
//      case StructToInterfaceUpcastH(lineH, sourceStructRef, targetInterfaceRef, resultTypeH, sourceLine, sourceLineType) => {
//        vfail("not yet")
//      }
//    }
//  }
//}
