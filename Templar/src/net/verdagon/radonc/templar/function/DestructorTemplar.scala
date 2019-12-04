package net.verdagon.radonc.templar.function

import net.verdagon.radonc.astronomer.FunctionA
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.templar.OverloadTemplar.{ScoutExpectedFunctionFailure, ScoutExpectedFunctionSuccess}
import net.verdagon.radonc.templar._
import net.verdagon.radonc.templar.env.{FunctionEnvironment, IEnvironment, ReferenceLocalVariable2, VariableId2}
import net.verdagon.radonc.{vassert, vfail, vimpl}

object DestructorTemplar {

  def getCitizenDestructor(
      env: IEnvironment,
      temputs0: Temputs,
      type2: Coord):
  (Temputs, Prototype2) = {
    type2.referend match {
      case PackT2(_, _) | StructRef2(_) => {// | OrdinaryClosure2(_, _, _) | TemplatedClosure2(_, _, _) => {
        OverloadTemplar.scoutExpectedFunctionForPrototype(
            env,
            temputs0,
            CallTemplar.DESTRUCTOR_NAME,
            List(),
            List(ParamFilter(type2, None)),
            true)  match {
          case (_, seff @ ScoutExpectedFunctionFailure(_, _, _, _, _)) => {
            vfail("Couldn't find concrete destructor!\n" + seff.toString)
          }
          case (temputs1, ScoutExpectedFunctionSuccess(p)) => (temputs1, p)
        }
      }
      case InterfaceRef2(_) => {
        OverloadTemplar.scoutExpectedFunctionForPrototype(
          env,
          temputs0,
          CallTemplar.INTERFACE_DESTRUCTOR_NAME,
          List(),
          List(ParamFilter(type2, None)),
          true) match {
          case (_, seff @ ScoutExpectedFunctionFailure(_, _, _, _, _)) => {
            vfail("Couldn't find interface destructor!\n" + seff.toString)
          }
          case (temputs1, ScoutExpectedFunctionSuccess(p)) => (temputs1, p)
        }
      }
    }
  }

  def getArrayDestructor(
    env: IEnvironment,
    temputs0: Temputs,
    type2: Coord):
  (Temputs, Prototype2) = {
    type2.referend match { case ArraySequenceT2(_, _) | UnknownSizeArrayT2(_) => }
    OverloadTemplar.scoutExpectedFunctionForPrototype(
      env,
      temputs0,
      CallTemplar.DESTRUCTOR_NAME,
      List(),
      List(ParamFilter(type2, None)),
      true) match {
      case (_, seff @ ScoutExpectedFunctionFailure(_, _, _, _, _)) => {
        vfail("Couldn't find array destructor!\n" + seff.toString)
      }
      case (temputs1, ScoutExpectedFunctionSuccess(p)) => (temputs1, p)
    }
  }

  // "Drop" is a general term that encompasses:
  // - Destruct. This means we take an owning reference and call its destructor.
  // - Unshare. This means we take a shared reference, and if it's the last one, unshare anything
  //   it's pointing at and deallocate.
  // - Unborrow. This is a no op.
  private def getDropFunction(
      env: IEnvironment,
      temputs0: Temputs,
      type2: Coord):
  (Temputs, Prototype2) = {
    OverloadTemplar.scoutExpectedFunctionForPrototype(
      env,
      temputs0,
      CallTemplar.DROP_FUNCTION_NAME,
      List(),
      List(ParamFilter(type2, None)),
      true) match {
      case (_, seff @ ScoutExpectedFunctionFailure(_, _, _, _, _)) => {
        vfail("Couldn't find drop function!\n" + seff.toString)
      }
      case (temputs1, ScoutExpectedFunctionSuccess(p)) => (temputs1, p)
    }
  }

  def generateDropFunction(
    innerEnv: FunctionEnvironment,
    temputsA: Temputs,
    originFunction1: FunctionA,
    type2: Coord):
  (Temputs, FunctionHeader2) = {
    val fateA = innerEnv

    val (temputsB, fateB, dropExpr2) =
      DestructorTemplar.drop(
        fateA,
        temputsA,
        ArgLookup2(0, type2))
    val header =
      FunctionHeader2(
        innerEnv.fullName,
        0,
        isExtern = false,
        isUserFunction = false,
        List(Parameter2("x", None, type2)),
        Coord(Raw, Void2()),
        Some(originFunction1))
    val function2 = Function2(header, List(), Block2(List(dropExpr2)))
    val temputsC =
      temputsB
        .declareFunctionReturnType(header.toSignature, Coord(Raw, Void2()))
        .addFunction(function2)
    vassert(temputsC.exactDeclaredSignatureExists(innerEnv.fullName, header.paramTypes))
    (temputsC, header)
  }

  def drop(
      fate0: FunctionEnvironment,
      temputs0: Temputs,
      undestructedExpr2: ReferenceExpression2):
  (Temputs, FunctionEnvironment, ReferenceExpression2) = {
    val (temputsX, fateX, resultExpr2) =
      undestructedExpr2.resultRegister.reference match {
        case Coord(Raw, _) => {
          undestructedExpr2.resultRegister.reference.referend match {
            case Void2() =>
            case Never2() =>
            case _ => {
              vfail("wat")
            }
          }
          (temputs0, fate0, undestructedExpr2)
        }
        case r @ Coord(Own, referend) => {
          val (temputs1, destructorPrototype) =
            referend match {
              case PackT2(_, understructRef) => {
                getCitizenDestructor(fate0, temputs0, Coord(Own, understructRef))
              }
              case StructRef2(_) | InterfaceRef2(_) => {
                getCitizenDestructor(fate0, temputs0, r)
              }
              case ArraySequenceT2(_, _) | UnknownSizeArrayT2(_) => {
                getArrayDestructor(fate0, temputs0, r)
              }
            }
          val (temputs2, fate2, destructExpr2) =
            CallTemplar.evaluatePrefixCall(
              temputs1,
              fate0,
              FunctionLookup2(destructorPrototype),
              List(),
              undestructedExpr2)
          vassert(destructExpr2.resultRegister.reference.referend == Void2())
          (temputs2, fate2, destructExpr2)
        }
        case Coord(Borrow, _) => (temputs0, fate0, Discard2(undestructedExpr2))
        case Coord(Share, _) => {
          val destroySharedCitizen =
            (temputs0: Temputs, Coord: Coord) => {
              val (temputs1, destructorHeader) =
                getCitizenDestructor(fate0, temputs0, Coord)
              // We just needed to ensure it's in the temputs, so that the backend can use it
              // for when reference counts drop to zero.
              // If/when we have a GC backend, we can skip generating share destructors.
              val _ = destructorHeader
              (temputs1, Discard2(undestructedExpr2))
            };
          val destroySharedArray =
            (temputs0: Temputs, Coord: Coord) => {
              val (temputs1, destructorHeader) =
                getArrayDestructor(fate0, temputs0, Coord)
              // We just needed to ensure it's in the temputs, so that the backend can use it
              // for when reference counts drop to zero.
              // If/when we have a GC backend, we can skip generating share destructors.
              val _ = destructorHeader
              (temputs1, Discard2(undestructedExpr2))
            };


          val (temputs2, unshareExpr2) =
            undestructedExpr2.resultRegister.reference.referend match {
              case Int2() | Str2() | Bool2() | Float2() => {
                (temputs0, Discard2(undestructedExpr2))
              }
              case as @ ArraySequenceT2(_, _) => {
                val underarrayReference2 = Coord(undestructedExpr2.resultRegister.reference.ownership, as)
                destroySharedArray(temputs0, underarrayReference2)
              }
              case as @ UnknownSizeArrayT2(_) => {
                val underarrayReference2 = Coord(undestructedExpr2.resultRegister.reference.ownership, as)
                destroySharedArray(temputs0, underarrayReference2)
              }
              case OverloadSet(overloadSetEnv, name, voidStructRef) => {
                val understructReference2 = undestructedExpr2.resultRegister.reference.copy(referend = voidStructRef)
                destroySharedCitizen(temputs0, understructReference2)
              }
              case PackT2(_, understruct2) => {
                val understructReference2 = undestructedExpr2.resultRegister.reference.copy(referend = understruct2)
                destroySharedCitizen(temputs0, understructReference2)
              }
//              case OrdinaryClosure2(_, sr, _) => {
//                val understructReference2 = undestructedExpr2.resultRegister.reference.copy(referend = sr)
//                destroySharedCitizen(temputs0, understructReference2)
//              }
//              case TemplatedClosure2(_, sr, _) => {
//                val understructReference2 = undestructedExpr2.resultRegister.reference.copy(referend = sr)
//                destroySharedCitizen(temputs0, understructReference2)
//              }
              case StructRef2(_) | InterfaceRef2(_) => {
                destroySharedCitizen(temputs0, undestructedExpr2.resultRegister.reference)
              }
            }
          (temputs2, fate0, unshareExpr2)
        }
      }
    vassert(
      resultExpr2.resultRegister.reference == Coord(Raw, Void2()) ||
      resultExpr2.resultRegister.reference == Coord(Raw, Never2()))
    (temputsX, fateX, resultExpr2)
  }

  def generateStructDestructor(
      innerEnv: FunctionEnvironment,
      temputs0: Temputs,
      originFunction1: FunctionA,
      params2: List[Parameter2],
      structRef: StructRef2):
  (Temputs, FunctionHeader2) = {
    val fate0 = innerEnv

    val destructorFullName = innerEnv.fullName

    val structDef = temputs0.lookupStruct(structRef)
    val structOwnership = if (structDef.mutability == Mutable) Own else Share
    val structBorrowOwnership = if (structDef.mutability == Mutable) Borrow else Share
    val structType = Coord(structOwnership, structDef.getRef)

    val header =
      FunctionHeader2(
        destructorFullName,
        0,
        false, false,
        params2,
        Coord(Raw, Void2()),
        Some(originFunction1));
    val temputs1 =
      temputs0
          .declareFunctionReturnType(header.toSignature, header.returnType)

    val structArgument = ArgLookup2(0, structType)
    val memberLocalVariables =
      structDef.members.flatMap({
        case StructMember2(name, variability, ReferenceMemberType2(reference)) => {
          List(ReferenceLocalVariable2(VariableId2(0, name), Final, reference))
        }
        case StructMember2(name, variability, AddressMemberType2(reference)) => {
          // See Destructure2 and its handling of addressible members for why
          // we don't include these in the destination variables.
          List()
        }
      })

    val destroyedUnletStruct = Destructure2(structArgument, structRef, memberLocalVariables)
    val (temputs4, fate4, destructMemberExprs) =
      memberLocalVariables.foldLeft((temputs1, fate0, List[ReferenceExpression2]()))({
        case ((temputs2, fate2, previousDestructMemberExprs), variable) => {
          val (temputs3, fate3, destructMemberExpr) =
            drop(fate2, temputs2, Unlet2(variable))
          (temputs3, fate3, previousDestructMemberExprs :+ destructMemberExpr)
        }
      })

    val _ = fate4

    val voidLiteral = VoidLiteral2()

    val function2 =
      Function2(
        header,
        memberLocalVariables,
        Block2(List(destroyedUnletStruct) ++ destructMemberExprs :+ voidLiteral))
    val temputs5 = temputs4.addFunction(function2)
    (temputs5, function2.header)
  }

  def generateArraySequenceDestructor(
    env: IEnvironment,
    temputs0: Temputs,
    maybeOriginFunction1: Option[FunctionA],
    sequenceRefType2: Coord,
    sequence: ArraySequenceT2):
  (Temputs, FunctionHeader2) = {
    val templatas = List(CoordTemplata(sequenceRefType2))
    val destructorFullName = FullName2(List(NamePart2("destructor", Some(templatas))))

    val arrayOwnership = if (sequence.array.mutability == Mutable) Own else Share
    val arrayBorrowOwnership = if (sequence.array.mutability == Mutable) Borrow else Share
    val arrayRefType = Coord(arrayOwnership, sequence)

    val (temputs1, elementDropFunctionPrototype) = getDropFunction(env, temputs0, sequence.array.memberType)

    val function2 =
      Function2(
        FunctionHeader2(
          destructorFullName,
          0,
          false, false,
          List(Parameter2("this", None, arrayRefType)),
          Coord(Raw, Void2()),
          maybeOriginFunction1),
        List(),
        Block2(
          List(
            DestroyArraySequence2(
              ArgLookup2(0, arrayRefType),
              sequence,
              FunctionPointerCall2(
                FunctionLookup2(elementDropFunctionPrototype),
                List(Placeholder2(sequence.array.memberType)))))))
    val temputs2 =
      temputs1
          .declareFunctionReturnType(function2.header.toSignature, function2.header.returnType)
          .addFunction(function2)
    (temputs2, function2.header)
  }

  def generateUnknownSizeArrayDestructor(
      env: IEnvironment,
      temputs0: Temputs,
      maybeOriginFunction1: Option[FunctionA],
      arrayRefType2: Coord,
      array: UnknownSizeArrayT2):
  (Temputs, FunctionHeader2) = {
    val templatas = List(CoordTemplata(arrayRefType2))
    val destructorFullName = FullName2(List(NamePart2("destructor", Some(templatas))))

    val arrayOwnership = if (array.array.mutability == Mutable) Own else Share
    val arrayBorrowOwnership = if (array.array.mutability == Mutable) Borrow else Share

    val (temputs1, elementDropFunctionPrototype) =
      getDropFunction(env, temputs0, array.array.memberType)

    val function2 =
      Function2(
        FunctionHeader2(
          destructorFullName,
          0,
          false, false,
          List(Parameter2("this", None, arrayRefType2)),
          Coord(Raw, Void2()),
          maybeOriginFunction1),
        List(),
        Block2(
          List(
            DestroyUnknownSizeArray2(
              ArgLookup2(0, arrayRefType2),
              array,
              FunctionPointerCall2(
                FunctionLookup2(elementDropFunctionPrototype),
                List(Placeholder2(array.array.memberType)))))))
    val temputs2 =
      temputs1
          .declareFunctionReturnType(function2.header.toSignature, function2.header.returnType)
          .addFunction(function2)
    (temputs2, function2.header)
  }
}
