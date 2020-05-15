package net.verdagon.vale.vivem

import net.verdagon.vale.metal._
import net.verdagon.vale.{vassert, vassertSome, vfail, vimpl, vwat, metal => m}

object ExpressionVivem {
  sealed trait INodeExecuteResult
  case class NodeContinue(resultRegister: Option[RegisterId]) extends INodeExecuteResult
  // None means VoidH
  case class NodeReturn(returnRef: Option[ReturnV]) extends INodeExecuteResult

  def executeNode(
      programH: ProgramH,
      stdin: (() => String),
      stdout: (String => Unit),
      heap: Heap,
      blockId: BlockId,
      node: NodeH):
  INodeExecuteResult = {
    val registerId = RegisterId(blockId, node.registerId)
    node match {
      case DiscardH(_, sourceRegister) => {
        sourceRegister.expectedType.ownership match {
          case Borrow | Share => {
            val ref =
              heap.takeReferenceFromRegister(
                RegisterId(blockId, sourceRegister.registerId),
                sourceRegister.expectedType)
            dropReference(programH, heap, stdout, stdin, blockId, ref)
          }
        }
        NodeContinue(None)
      }
      case ConstantVoidH(_) => {
        // Do nothing, a void can never exist
        NodeContinue(None)
      }
      case UnreachableH(_) => {
        vfail()
        NodeContinue(Some(registerId))
      }
      case ConstantI64H(_, value) => {
        heap.allocateIntoRegister(registerId, Share, IntV(value))
        NodeContinue(Some(registerId))
      }
      case ConstantF64H(_, value) => {
        heap.allocateIntoRegister(registerId, Share, FloatV(value))
        NodeContinue(Some(registerId))
      }
      case ConstantStrH(_, value) => {
        heap.allocateIntoRegister(registerId, Share, StrV(value))
        NodeContinue(Some(registerId))
      }
      case ConstantBoolH(_, value) => {
        heap.allocateIntoRegister(registerId, Share, BoolV(value))
        NodeContinue(Some(registerId))
      }
      case ArgumentH(_, resultType, argumentIndex) => {
        heap.moveArgumentIntoRegister(registerId, argumentIndex, resultType)
        NodeContinue(Some(registerId))
      }
      case ReturnH(_, sourceRegister) => {
        heap.vivemDout.print("flort")
        if (sourceRegister.expectedType.kind == VoidH()) {
          return NodeReturn(None)
        } else {
          return NodeReturn(Some(heap.returnFromRegister(RegisterId(blockId, sourceRegister.registerId), sourceRegister.expectedType)))
        }
      }
      case CheckRefCountH(_, refRegister, category, numRegister) => {
        val numReference = heap.takeReferenceFromRegister(RegisterId(blockId, numRegister.registerId), numRegister.expectedType)
        val refReference = heap.takeReferenceFromRegister(RegisterId(blockId, refRegister.registerId), refRegister.expectedType)
        val num =
          heap.dereference(numReference) match {
            case IntV(n) => n
          }
        heap.ensureRefCount(refReference, category, num)

        dropReferenceIfNonOwning(programH, heap, stdout, stdin, blockId, numReference)
        NodeContinue(None)
      }
//      case SoftLoadH(_, sourceRegister, targetOwnership) => {
//        val sourceRegisterId = RegisterId(blockId, sourceRegister.registerId)
//        val address = heap.takeAddressFromRegister(sourceRegisterId, sourceRegister.expectedType)
//        heap.vivemDout.print(" *")
//        printAddress(heap.vivemDout, address)
//        val source = heap.dereferenceAddress(address, sourceRegister.expectedType)
//        if (targetOwnership == Own) {
//          heap.setReferenceRegister(registerId, source)
//          heap.blacklistAddress(address, sourceRegister.expectedType)
//        } else {
//          heap.aliasIntoRegister(
//            registerId,
//            source,
//            sourceRegister.expectedType,
//            targetOwnership)
//        }
//        heap.maybeDeallocateAddressRegister(sourceRegisterId, address)
//      }
      case InlineBlockH(_, blockH) => {
        BlockVivem.executeBlock(programH, stdin, stdout, heap, blockId.callId, blockH) match {
          case BlockReturn(returnRef) => NodeReturn(returnRef)
          case BlockContinue(None) => NodeContinue(None)
          case BlockContinue(Some(resultRef)) => {
            heap.setReferenceRegisterFromReturn(registerId, resultRef)
            NodeContinue(Some(registerId))
          }
        }
      }
      case DestructureH(_, structRegister, localTypes, locals) => {
        val structReference = heap.takeReferenceFromRegister(RegisterId(blockId, structRegister.registerId), structRegister.expectedType)
        heap.ensureTotalRefCount(structReference, 0)

        val oldMemberReferences = heap.destructure(structReference)

        vassert(oldMemberReferences.size == locals.size)
        oldMemberReferences.zip(localTypes).zip(locals).foreach({ case ((memberRef, localType), localIndex) =>
          val varAddr = heap.getVarAddress(blockId.callId, localIndex)
          heap.addLocal(varAddr, memberRef, localType)
          heap.vivemDout.print(" v" + varAddr + "<-o" + memberRef.num)
        })
        NodeContinue(None)
      }
      case ArrayLengthH(_, arrayRegister) => {
        val arrayReference = heap.takeReferenceFromRegister(RegisterId(blockId, arrayRegister.registerId), arrayRegister.expectedType)
        val arr @ ArrayInstanceV(_, _, _, _) = heap.dereference(arrayReference)

        heap.allocateIntoRegister(registerId, Share, IntV(arr.getSize()))
        NodeContinue(Some(registerId))
      }
      case StackifyH(_, sourceRegister, localIndex, name) => {
        val reference = heap.takeReferenceFromRegister(RegisterId(blockId, sourceRegister.registerId), sourceRegister.expectedType)
        val varAddr = heap.getVarAddress(blockId.callId, localIndex)
        heap.addLocal(varAddr, reference, sourceRegister.expectedType)
        heap.vivemDout.print(" v" + varAddr + "<-o" + reference.num)
        NodeContinue(None)
      }
//      case LocalLookupH(_, localIndex, expectedType, name) => {
//        // Check that its there
//        heap.getReferenceFromLocal(VariableAddressV(callId, localIndex), expectedType)
//
//        heap.setVariableAddressRegister(registerId, VariableAddressV(callId, localIndex))
//      }

      case LocalStoreH(_, localIndex, sourceRegister, name) => {
        val varAddress = heap.getVarAddress(blockId.callId, localIndex)
        val reference = heap.takeReferenceFromRegister(RegisterId(blockId, sourceRegister.registerId), sourceRegister.expectedType)
        heap.vivemDout.print(" " + varAddress + "(\"" + name + "\")")
        heap.vivemDout.print("<-" + reference.num)
        val oldRef = heap.mutateVariable(varAddress, reference, sourceRegister.expectedType)
        heap.setReferenceRegister(registerId, oldRef)
        NodeContinue(Some(registerId))
      }

      case MemberStoreH(_, structRegister, memberIndex, sourceRegister, memberName) => {
        val structReference = heap.takeReferenceFromRegister(RegisterId(blockId, structRegister.registerId), structRegister.expectedType)
        val address = MemberAddressV(structReference.allocId, memberIndex)
        val reference = heap.takeReferenceFromRegister(RegisterId(blockId, sourceRegister.registerId), sourceRegister.expectedType)
        heap.vivemDout.print(" " + address + "(\"" + memberName + "\")")
        heap.vivemDout.print("<-" + reference.num)
        val oldMemberReference = heap.mutateStruct(address, reference, sourceRegister.expectedType)
        heap.setReferenceRegister(registerId, oldMemberReference)
        NodeContinue(Some(registerId))
      }

      case UnknownSizeArrayStoreH(_, structRegister, indexRegister, sourceRegister) => {
        val indexReference = heap.takeReferenceFromRegister(RegisterId(blockId, indexRegister.registerId), indexRegister.expectedType)
        val arrayReference = heap.takeReferenceFromRegister(RegisterId(blockId, structRegister.registerId), structRegister.expectedType)
        val IntV(elementIndex) = heap.dereference(indexReference)

        val address = ElementAddressV(arrayReference.allocId, elementIndex)
        val reference = heap.takeReferenceFromRegister(RegisterId(blockId, sourceRegister.registerId), sourceRegister.expectedType)
        heap.vivemDout.print(" " + address)
        heap.vivemDout.print("<-" + reference.num)
        val oldMemberReference = heap.mutateArray(address, reference, sourceRegister.expectedType)
        heap.setReferenceRegister(registerId, oldMemberReference)

        dropReferenceIfNonOwning(programH, heap, stdout, stdin, blockId, indexReference)
        NodeContinue(Some(registerId))
      }

      case KnownSizeArrayStoreH(_, structRegister, indexRegister, sourceRegister) => {
        val indexReference = heap.takeReferenceFromRegister(RegisterId(blockId, indexRegister.registerId), indexRegister.expectedType)
        val arrayReference = heap.takeReferenceFromRegister(RegisterId(blockId, structRegister.registerId), structRegister.expectedType)
        val IntV(elementIndex) = heap.dereference(indexReference)

        val address = ElementAddressV(arrayReference.allocId, elementIndex)
        val reference = heap.takeReferenceFromRegister(RegisterId(blockId, sourceRegister.registerId), sourceRegister.expectedType)
        heap.vivemDout.print(" " + address)
        heap.vivemDout.print("<-" + reference.num)
        val oldMemberReference = heap.mutateArray(address, reference, sourceRegister.expectedType)
        heap.setReferenceRegister(registerId, oldMemberReference)
        NodeContinue(Some(registerId))
      }

      case LocalLoadH(_, localIndex, targetOwnership, expectedLocalType, expectedResultType, name) => {
        vassert(targetOwnership != Own) // should have been Unstackified instead
        val varAddress = heap.getVarAddress(blockId.callId, localIndex)
        val reference = heap.getReferenceFromLocal(varAddress, expectedLocalType)
        heap.vivemDout.print(" *" + varAddress)
        heap.aliasIntoRegister(
          registerId,
          reference,
          expectedLocalType,
          targetOwnership)
        NodeContinue(Some(registerId))
      }

      case UnstackifyH(_, localIndex, expectedType) => {
        val varAddress = heap.getVarAddress(blockId.callId, localIndex)
        val reference = heap.getReferenceFromLocal(varAddress, expectedType)
        heap.vivemDout.print(" ^" + varAddress)
        heap.setReferenceRegister(registerId, reference)
        heap.removeLocal(varAddress, expectedType)
        NodeContinue(Some(registerId))
      }
      case CallH(_, functionRef, argsRegisters) => {
        if (programH.functions.find(_.prototype == functionRef).get.isExtern) {

          val externFunction = FunctionVivem.getExternFunction(programH, functionRef)
          val argReferences =
            heap.takeReferencesFromRegistersInReverse(blockId, argsRegisters)

          val maybeResultReference =
            externFunction(
              new AdapterForExterns(
                programH, heap, blockId, stdin, stdout,
                (reference) => {
                  dropReferenceIfNonOwning(programH, heap, stdout, stdin, blockId, reference)
                }),
              argReferences.toVector)

          // Special case for externs; externs arent allowed to change ref counts at all
          //        argReferences.foreach(heap.maybeDeallocate)

          (functionRef.returnType.kind, maybeResultReference) match {
            case (VoidH(), None) => NodeContinue(None)
            case (_, Some(resultReference)) => {
              heap.setReferenceRegisterFromReturn(registerId, resultReference)
              NodeContinue(Some(registerId))
            }
          }
        } else {
          val argReferences = heap.takeReferencesFromRegistersInReverse(blockId, argsRegisters)
          heap.vivemDout.println()
          heap.vivemDout.println("  " * blockId.blockHeight + "Making new stack frame (call)")

          val function =
            programH.functions.find(_.prototype == functionRef).get

          val maybeReturnReference =
            FunctionVivem.executeFunction(
              programH, stdin, stdout, heap, argReferences.toVector, function)
          heap.vivemDout.print("  " * blockId.blockHeight + "Getting return reference")
          (functionRef.returnType.kind, maybeReturnReference) match {
            case (VoidH(), None) => NodeContinue(None)
            case (_, Some(returnReference)) => {
              heap.setReferenceRegisterFromReturn(registerId, returnReference)
              NodeContinue(Some(registerId))
            }
          }
        }
      }
      case NewStructH(_, sourceLines, structRefH) => {
        val structDefH = programH.structs.find(_.getRef == structRefH.kind).get

        val memberReferences =
          heap.takeReferencesFromRegistersInReverse(blockId, sourceLines)

        vassert(memberReferences.size == structDefH.members.size)
        val reference = heap.newStruct(registerId, structDefH, structRefH, memberReferences)
        heap.setReferenceRegister(registerId, reference)
        NodeContinue(Some(registerId))
      }
      case NewArrayFromValuesH(_, sourceRegisters, arrayRefType) => {
        val refs = heap.takeReferencesFromRegistersInReverse(blockId, sourceRegisters)

        val (arrayReference, arrayInstance) =
          heap.addArray(arrayRefType, refs)

        heap.vivemDout.print(" o" + arrayReference.num + "=")
        heap.printReferend(arrayInstance)
        heap.setReferenceRegister(registerId, arrayReference)
        NodeContinue(Some(registerId))
      }

      case MemberLoadH(_, structRegister, memberIndex, targetOwnership, expectedMemberType, expectedResultType, memberName) => {
        val structReference = heap.takeReferenceFromRegister(RegisterId(blockId, structRegister.registerId), structRegister.expectedType)

        val address = MemberAddressV(structReference.allocId, memberIndex)

        heap.vivemDout.print(" *" + address)
        val source = heap.getReferenceFromStruct(address, expectedMemberType)
        vassert(targetOwnership != Own)
        heap.aliasIntoRegister(
          registerId,
          source,
          expectedMemberType,
          targetOwnership)

        dropReferenceIfNonOwning(programH, heap, stdout, stdin, blockId, structReference)
        NodeContinue(Some(registerId))
      }

      case UnknownSizeArrayLoadH(_, arrayRegister, indexRegister, resultType, targetOwnership) => {
        val indexIntReference =
          heap.takeReferenceFromRegister(
            RegisterId(blockId, indexRegister.registerId), ReferenceH(m.Share, IntH()))
        val arrayReference =
          heap.takeReferenceFromRegister(
            RegisterId(blockId, arrayRegister.registerId), arrayRegister.expectedType)
        val index =
          heap.dereference(indexIntReference) match {
            case IntV(value) => value
          }

        val address = ElementAddressV(arrayReference.allocId, index)

        heap.vivemDout.print(" *" + address)
        val source = heap.getReferenceFromArray(address, arrayRegister.expectedType.kind.rawArray.elementType)
        if (targetOwnership == Own) {
          vfail("impl me?")
        } else {
          heap.aliasIntoRegister(
            registerId,
            source,
            arrayRegister.expectedType.kind.rawArray.elementType,
            targetOwnership)
        }

        dropReferenceIfNonOwning(programH, heap, stdout, stdin, blockId, indexIntReference)
        dropReferenceIfNonOwning(programH, heap, stdout, stdin, blockId, arrayReference)
        NodeContinue(Some(registerId))
      }

      case KnownSizeArrayLoadH(_, arrayRegister, indexRegister, resultType, targetOwnership) => {
        val indexIntReference =
          heap.takeReferenceFromRegister(
            RegisterId(blockId, indexRegister.registerId), ReferenceH(m.Share, IntH()))
        val arrayReference =
          heap.takeReferenceFromRegister(
            RegisterId(blockId, arrayRegister.registerId), arrayRegister.expectedType)
        val index =
          heap.dereference(indexIntReference) match {
            case IntV(value) => value
          }

        val address = ElementAddressV(arrayReference.allocId, index)

        heap.vivemDout.print(" *" + address)
        val source = heap.getReferenceFromArray(address, arrayRegister.expectedType.kind.rawArray.elementType)
        if (targetOwnership == Own) {
          vfail("impl me?")
        } else {
          heap.aliasIntoRegister(
            registerId,
            source,
            arrayRegister.expectedType.kind.rawArray.elementType,
            targetOwnership)
        }

        dropReferenceIfNonOwning(programH, heap, stdout, stdin, blockId, indexIntReference)
        dropReferenceIfNonOwning(programH, heap, stdout, stdin, blockId, arrayReference)
        NodeContinue(Some(registerId))
      }
      case siu @ StructToInterfaceUpcastH(_, sourceRegister, targetInterfaceRef) => {
        val sourceReference = heap.takeReferenceFromRegister(RegisterId(blockId, sourceRegister.registerId), sourceRegister.expectedType);
        val ownership = sourceReference.ownership

        val targetReference =
          ReferenceV(
            sourceReference.actualKind,
            RRReferend(targetInterfaceRef),
            sourceReference.ownership,
            sourceReference.num)

        heap.aliasIntoRegister(registerId, targetReference, siu.resultRef, ownership)
        NodeContinue(Some(registerId))
      }
      case icH @ InterfaceCallH(_, argsRegisters, virtualParamIndex, interfaceRefH, indexInEdge, functionType) => {

        // undeviewed = not deviewed = the virtual param is still a view and we want it to
        // be a struct.
        val undeviewedArgReferences = heap.takeReferencesFromRegistersInReverse(blockId, argsRegisters)

        heap.vivemDout.println()
        heap.vivemDout.println("  " * blockId.blockHeight + "Making new stack frame (icall)")

        val (functionH, maybeReturnReference) =
          executeInterfaceFunction(programH, stdin, stdout, heap, undeviewedArgReferences, virtualParamIndex, interfaceRefH, indexInEdge, functionType)

        (functionH.prototype.returnType.kind, maybeReturnReference) match {
          case (VoidH(), None) => NodeContinue(None)
          case (_, Some(returnReference)) => {
            heap.setReferenceRegisterFromReturn(registerId, returnReference)
            NodeContinue(Some(registerId))
          }
        }
      }
      case IfH(_, conditionBlock, thenBlock, elseBlock) => {
        val conditionBlockResult = BlockVivem.executeBlock(programH, stdin, stdout, heap, blockId.callId, conditionBlock)
        val BlockContinue(Some(returnV)) = conditionBlockResult
        val conditionReference = heap.getReferenceFromReturn(returnV)
        val conditionReferend = heap.dereference(conditionReference)
        val BoolV(conditionValue) = conditionReferend;

        dropReferenceIfNonOwning(programH, heap, stdout, stdin, blockId, conditionReference)

        val blockResult =
          if (conditionValue == true) {
            BlockVivem.executeBlock(programH, stdin, stdout, heap, blockId.callId, thenBlock)
          } else {
            BlockVivem.executeBlock(programH, stdin, stdout, heap, blockId.callId, elseBlock)
          }
        blockResult match {
          case BlockReturn(returnRef) => NodeReturn(returnRef)
          case BlockContinue(None) => NodeContinue(None)
          case BlockContinue(Some(resultRef)) => {
            heap.setReferenceRegisterFromReturn(registerId, resultRef)
            NodeContinue(Some(registerId))
          }
        }
      }
      case WhileH(_, bodyBlock) => {
        var continue = true
        while (continue) {
          val conditionBlockResult =
            BlockVivem.executeBlock(programH, stdin, stdout, heap, blockId.callId, bodyBlock)

          conditionBlockResult match {
            case BlockReturn(maybeReturnRef) => {
              return NodeReturn(maybeReturnRef)
            }
            case BlockContinue(Some(returnV)) => {
              val conditionReference = heap.getReferenceFromReturn(returnV)
              val conditionReferend = heap.dereference(conditionReference)
              val BoolV(conditionValue) = conditionReferend;
              continue = conditionValue
              dropReference(programH, heap, stdout, stdin, blockId, conditionReference)
            }
          }
        }
        NodeContinue(None)
      }
      case cac @ ConstructUnknownSizeArrayH(_, sizeRegister, generatorInterfaceRegister, arrayRefType) => {
        val generatorInterfaceRef =
          heap.takeReferenceFromRegister(
            RegisterId(blockId, generatorInterfaceRegister.registerId), generatorInterfaceRegister.expectedType)

        val sizeReference = heap.takeReferenceFromRegister(RegisterId(blockId, sizeRegister.registerId), ReferenceH(m.Share, IntH()))
        val sizeReferend = heap.dereference(sizeReference)
        val IntV(size) = sizeReferend;
        val (arrayReference, arrayInstance) =
          heap.addUninitializedArray(arrayRefType, size)

        (0 until size).foreach(i => {
          heap.vivemDout.println()
          heap.vivemDout.println("  " * blockId.blockHeight + "Making new stack frame (generator)")

          val indexReference = heap.allocateTransient(Share, IntV(i))

          // We're assuming here that theres only 1 method in the interface.
          val indexInEdge = 0
          // We're assuming that it takes self then the index int as arguments.
          val virtualParamIndex = 0

          val interfaceDefH =
            programH.interfaces.find(_.getRef == generatorInterfaceRegister.expectedType.kind).get
          val interfaceMethodPrototype = interfaceDefH.prototypes.head

          heap.vivemDout.println()

          heap.vivemDout.println()
          heap.vivemDout.println("  " * blockId.blockHeight + "Making new stack frame (icall)")

          val (functionH, maybeReturnReference) =
            executeInterfaceFunction(
              programH,
              stdin,
              stdout,
              heap,
              List(generatorInterfaceRef, indexReference),
              virtualParamIndex,
              generatorInterfaceRegister.expectedType.kind,
              indexInEdge,
              interfaceMethodPrototype)

          heap.vivemDout.print("  " * blockId.blockHeight + "Getting return reference")

          vassert(maybeReturnReference.nonEmpty)
          val returnReference = maybeReturnReference.get

          // No need to increment or decrement, we're conceptually moving the return value
          // from the return slot to the array slot
          heap.initializeArrayElementFromReturn(arrayReference, i, returnReference)
        });


        //        heap.allocateIntoRegister(registerId, arrayRefType.ownership, arrayInstance)
        //        val arrayReference = heap.takeReferenceFromRegister(registerId, arrayRefType)

        heap.vivemDout.print(" o" + arrayReference.num + "=")
        heap.printReferend(arrayInstance)
        heap.setReferenceRegister(registerId, arrayReference)

        //        heap.maybeDeallocate(thrownAwayGeneratorIndexArgReference)
        //        heap.maybeDeallocate(sizeReference)
        //        heap.maybeDeallocate(arrayReference)
        //        heap.maybeDeallocate(generatorFunctionRef)

        NodeContinue(Some(registerId))
      }

      case DestroyKnownSizeArrayH(_, arrayRegister, consumerInterfaceRegister) => {
        val consumerInterfaceRef =
          heap.takeReferenceFromRegister(
            RegisterId(blockId, consumerInterfaceRegister.registerId), consumerInterfaceRegister.expectedType)

        val arrayReference =
          heap.takeReferenceFromRegister(RegisterId(blockId, arrayRegister.registerId), arrayRegister.expectedType)
        heap.ensureTotalRefCount(arrayReference, 0)

        val size = arrayRegister.expectedType.kind.size
        (0 until size).foreach(ascendingI => {
          val i = size - ascendingI - 1

          heap.vivemDout.println()
          heap.vivemDout.println("  " * blockId.blockHeight + "Making new stack frame (consumer)")

          val indexReference = heap.allocateTransient(Share, IntV(i))

          // We're assuming here that theres only 1 method in the interface.
          val indexInEdge = 0
          // We're assuming that it takes self then the index int as arguments.
          val virtualParamIndex = 0

          val consumerPrototypeH =
            PrototypeH(
              vimpl(),//FullNameH(List(NamePartH("__call", None, None, None))),
              List(consumerInterfaceRegister.expectedType, ReferenceH(Share, IntH())),
              arrayRegister.expectedType.kind.rawArray.elementType)

          heap.vivemDout.println()

          heap.vivemDout.println()
          heap.vivemDout.println("  " * blockId.blockHeight + "Making new stack frame (icall)")

          val (functionH, maybeReturnReference) =
            executeInterfaceFunction(
              programH,
              stdin,
              stdout,
              heap,
              List(consumerInterfaceRef, indexReference),
              virtualParamIndex,
              consumerInterfaceRegister.expectedType.kind,
              indexInEdge,
              consumerPrototypeH)

          // This instruction is specifically about NOT producing a new array. The return
          // better be empty.
          vassert(maybeReturnReference.isEmpty)
        });

        heap.deallocate(arrayReference)
        NodeContinue(None)
      }

      case cac @ DestroyUnknownSizeArrayH(_, arrayRegister, consumerInterfaceRegister) => {
        val consumerInterfaceRef =
          heap.takeReferenceFromRegister(
            RegisterId(blockId, consumerInterfaceRegister.registerId), consumerInterfaceRegister.expectedType)

        val arrayReference =
          heap.takeReferenceFromRegister(RegisterId(blockId, arrayRegister.registerId), arrayRegister.expectedType)
        heap.ensureTotalRefCount(arrayReference, 0)

        val ArrayInstanceV(_, _, size, _) = heap.dereference(arrayReference)

        (0 until size).foreach(ascendingI => {
          val i = size - ascendingI - 1

          heap.vivemDout.println()
          heap.vivemDout.println("  " * blockId.blockHeight + "Making new stack frame (consumer)")

          val indexReference = heap.allocateTransient(Share, IntV(i))

          // We're assuming here that theres only 1 method in the interface.
          val indexInEdge = 0
          // We're assuming that it takes self then the index int as arguments.
          val virtualParamIndex = 0

          val consumerPrototypeH =
            PrototypeH(
              vimpl(),//FullNameH(List(NamePartH("__call", None, None, None))),
              List(consumerInterfaceRegister.expectedType, ReferenceH(Share, IntH())),
              arrayRegister.expectedType.kind.rawArray.elementType)

          heap.vivemDout.println()

          heap.vivemDout.println()
          heap.vivemDout.println("  " * blockId.blockHeight + "Making new stack frame (icall)")

          val (functionH, maybeReturnReference) =
            executeInterfaceFunction(
              programH,
              stdin,
              stdout,
              heap,
              List(consumerInterfaceRef, indexReference),
              virtualParamIndex,
              consumerInterfaceRegister.expectedType.kind,
              indexInEdge,
              consumerPrototypeH)

          // This instruction is specifically about NOT producing a new array. The return
          // better be empty.
          vassert(maybeReturnReference.isEmpty)
        });

        heap.deallocate(arrayReference)
        NodeContinue(None)
      }
    }
  }

  private def executeInterfaceFunction(
      programH: ProgramH,
      stdin: () => String,
      stdout: String => Unit,
      heap: Heap,
      undeviewedArgReferences: List[ReferenceV],
      virtualParamIndex: Int,
      interfaceRefH: InterfaceRefH,
      indexInEdge: Int,
      functionType: PrototypeH) = {

    val interfaceReference = undeviewedArgReferences(virtualParamIndex)

    val StructInstanceV(structH, _) = heap.dereference(interfaceReference)

    val edge = structH.edges.find(_.interface == interfaceRefH).get

    val ReferenceV(actualStruct, actualInterfaceKind, actualOwnership, allocNum) = interfaceReference
    vassert(actualInterfaceKind.hamut == interfaceRefH)
    val structReference = ReferenceV(actualStruct, actualStruct, actualOwnership, allocNum)

    val prototypeH = edge.structPrototypesByInterfacePrototype.values.toList(indexInEdge)
    val functionH = programH.functions.find(_.prototype == prototypeH).get;

    val actualPrototype = functionH.prototype
    val expectedPrototype = functionType
    // We would compare functionH.type to functionType directly, but
    // functionH.type expects a struct and prototypeH expects an interface.

    // First, check that all the other params are correct.
    undeviewedArgReferences.zipWithIndex.zip(actualPrototype.params).zip(expectedPrototype.params).foreach({
      case (((argReference, index), actualFunctionParamType), expectedFunctionParamType) => {
        // Skip the interface line for now, we check it below
        if (index != virtualParamIndex) {
          heap.checkReference(actualFunctionParamType, argReference)
          heap.checkReference(expectedFunctionParamType, argReference)
          vassert(actualFunctionParamType == expectedFunctionParamType)
        }
      }
    })

    val deviewedArgReferences = undeviewedArgReferences.updated(virtualParamIndex, structReference)

    val maybeReturnReference =
      FunctionVivem.executeFunction(
        programH,
        stdin,
        stdout,
        heap,
        deviewedArgReferences.toVector,
        functionH)
    (functionH, maybeReturnReference)
  }

  def dropReferenceIfNonOwning(
      programH: ProgramH,
      heap: Heap,
      stdout: String => Unit,
      stdin: () => String,
      blockId: BlockId,
      reference: ReferenceV) = {
    reference.ownership match {
      case Own =>
      case Borrow | Share => {
        dropReference(programH, heap, stdout, stdin, blockId, reference)
      }
    }
  }

  private def dropReference(
      programH: ProgramH,
      heap: Heap,
      stdout: String => Unit,
      stdin: () => String,
      blockId: BlockId,
      reference: ReferenceV):
  Unit = {
    if (heap.getTotalRefCount(reference) == 0) {
      // If it's a share ref, then use runtime information to crawl through and decrement
      // ref counts.
      reference.ownership match {
        case Share => {
          reference.actualKind.hamut match {
            case InterfaceRefH(_) => {
              // We don't expect this because we asked for the actualKind, not the seenAsKind.
              vwat()
            }
            case IntH() | StrH() | BoolH() | FloatH() => {
              heap.deallocate(reference)
            }
            case StructRefH(_) => {
              // Deallocates the thing.
              val references = heap.destructure(reference)
              references.foreach(dropReference(programH, heap, stdout, stdin, blockId, _))
            }
            case UnknownSizeArrayTH(_) => {
              val ArrayInstanceV(_, _, _, elements) = heap.dereference(reference)
              val references = elements.indices.map(heap.deinitializeArrayElement(reference, _))
              heap.deallocate(reference)
              references.foreach(dropReference(programH, heap, stdout, stdin, blockId, _))
            }
            case KnownSizeArrayTH(_, _) => {
              val ArrayInstanceV(_, _, _, elements) = heap.dereference(reference)
              val references = elements.indices.map(heap.deinitializeArrayElement(reference, _))
              heap.deallocate(reference)
              references.foreach(dropReference(programH, heap, stdout, stdin, blockId, _))
            }
          }
        }
        case Own => {
          vimpl()
        }
      }
    }
  }
}
