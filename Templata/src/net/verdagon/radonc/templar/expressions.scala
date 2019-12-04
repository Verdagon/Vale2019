package net.verdagon.radonc.templar

import net.verdagon.radonc.scout.RefCountCategory
import net.verdagon.radonc.templar.env.{ILocalVariable2, ReferenceLocalVariable2, VariableId2}
import net.verdagon.radonc.templar.templata._
import net.verdagon.radonc.templar.types._
import net.verdagon.radonc.{vassert, vfail}

trait IRegister2 extends Queriable2 {
  def expectReference(): ReferenceRegister2 = {
    this match {
      case r @ ReferenceRegister2(_) => r
      case AddressRegister2(_) => vfail("Expected a reference as a result, but got an address!")
    }
  }
  def expectAddress(): AddressRegister2 = {
    this match {
      case a @ AddressRegister2(_) => a
      case ReferenceRegister2(_) => vfail("Expected an address as a result, but got a reference!")
    }
  }
  def underlyingReference: Coord
  def referend: Kind
}
case class AddressRegister2(reference: Coord) extends IRegister2 {
  override def underlyingReference: Coord = reference
  override def referend = reference.referend
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ reference.all(func)
  }
}
case class ReferenceRegister2(reference: Coord) extends IRegister2 {
  override def underlyingReference: Coord = reference
  override def referend = reference.referend
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ reference.all(func)
  }
}
trait Expression2 extends Queriable2 {
  def resultRegister: IRegister2
  def referend: Kind
}
trait ReferenceExpression2 extends Expression2 {
  override def resultRegister: ReferenceRegister2
  override def referend = resultRegister.reference.referend
}
trait AddressExpression2 extends Expression2 {
  override def resultRegister: AddressRegister2
  override def referend = resultRegister.reference.referend

  // Whether to move-load or borrow-load from this expression, if coerced
  // into a reference.
  def coerceToBorrow: Boolean
}

case class LetAndLend2(
    variable: ILocalVariable2,
    expr: ReferenceExpression2
) extends ReferenceExpression2 {
  vassert(variable.reference == expr.resultRegister.reference)

  override def resultRegister: ReferenceRegister2 = {
    val Coord(ownership, kind) = expr.resultRegister.reference
    ReferenceRegister2(Coord(if (ownership == Share) Share else Borrow, kind))
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ expr.all(func)
  }
}

case class LetNormal2(
    variable: ILocalVariable2,
    expr: ReferenceExpression2
) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Coord(Raw, Void2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ expr.all(func)
  }
}

// Only ExpressionTemplar.unletLocal should make these
case class Unlet2(variable: ILocalVariable2) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(variable.reference)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ variable.reference.all(func)
  }
}

// Throws away a reference.
// Unless given to an instruction which consumes it, all borrow and share
// references must eventually hit a Discard2, just like all owning
// references must eventually hit a Destructure2.
// Depending on the backend, it will either be a no-op (like for GC'd backends)
// or a decrement+maybedestruct (like for RC'd backends)
case class Discard2(
  expr: ReferenceExpression2
) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Coord(Raw, Void2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ expr.all(func)
  }
}

case class Defer2(
    innerExpr: ReferenceExpression2,
    deferredExpr: ReferenceExpression2) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(innerExpr.resultRegister.reference)

  vassert(deferredExpr.resultRegister.reference == Coord(Raw, Void2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ innerExpr.all(func) ++ deferredExpr.all(func)
  }
}

// Eventually, when we want to do if-let, we'll have a different construct
// entirely. See comment below If2.
// These are blocks because we don't want inner locals to escape.
case class If2(
    condition: Block2,
    thenCall: Block2,
    elseCall: Block2) extends ReferenceExpression2 {
  private val conditionResultCoord = condition.lastReferenceExpr.resultRegister.reference
  private val thenResultCoord = thenCall.lastReferenceExpr.resultRegister.reference
  private val elseResultCoord = elseCall.lastReferenceExpr.resultRegister.reference

  vassert(conditionResultCoord == Coord(Share, Bool2()))
  vassert(
    thenResultCoord.referend == Never2() ||
      elseResultCoord.referend == Never2() ||
      thenResultCoord == elseResultCoord)

  private val commonSupertype =
    if (thenResultCoord.referend == Never2()) {
      elseResultCoord
    } else {
      thenResultCoord
    }

  override def resultRegister = ReferenceRegister2(commonSupertype)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ condition.all(func) ++ thenCall.all(func) ++ elseCall.all(func)
  }
}

// case class IfLet2
// This would check whether:
// - The nullable condition expression evaluates to not null, or
// - The interface condition expression evaluates to the specified sub-citizen
// It would have to use a new chunk of PatternTemplar which produces an
// expression which is a ton of if-statements and try-cast things and assigns
// variables, and puts the given body inside all that.


// The block is expected to return a boolean (false = stop, true = keep going).
// The block will probably contain an If2(the condition, the body, false)
case class While2(block: Block2) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Coord(Raw, Void2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ block.all(func)
  }
}

case class Mutate2(
  destinationExpr: AddressExpression2,
  sourceExpr: ReferenceExpression2
) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(destinationExpr.resultRegister.reference)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ destinationExpr.all(func) ++ sourceExpr.all(func)
  }
}


case class Return2(
  sourceExpr: ReferenceExpression2
) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Coord(Raw, Never2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ sourceExpr.all(func)
  }
}


//case class CurriedFunc3(closureExpr: Expression3, funcName: String) extends Expression3

// when we make a closure, we make a struct full of pointers to all our variables
// and the first element is our parent closure
// this can live on the stack, since blocks are limited to this expression
// later we can optimize it to only have the things we use

// Block2 is required to unlet all the variables it introduces.
case class Block2(exprs: List[ReferenceExpression2]) extends ReferenceExpression2 {
  vassert(exprs.last.isInstanceOf[ReferenceExpression2])
  def lastReferenceExpr = exprs.last
  override def resultRegister = lastReferenceExpr.resultRegister

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ exprs.flatMap(_.all(func))
  }
}

case class Consecutor2(exprs: List[ReferenceExpression2]) extends ReferenceExpression2 {
  def lastReferenceExpr = exprs.last
  override def resultRegister = lastReferenceExpr.resultRegister

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ exprs.flatMap(_.all(func))
  }
}

case class PackE2(
    elements: List[ReferenceExpression2],
    resultReference: Coord,
    packType: PackT2) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(resultReference)
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ elements.flatMap(_.all(func)) ++ packType.all(func)
  }
}

case class TupleE2(
    elements: List[ReferenceExpression2],
    resultReference: Coord,
    tupleType: TupleT2) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(resultReference)
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ elements.flatMap(_.all(func)) ++ tupleType.all(func)
  }
}

case class ArraySequenceE2(
    elements: List[ReferenceExpression2],
    resultReference: Coord,
    arrayType: ArraySequenceT2) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(resultReference)
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ elements.flatMap(_.all(func)) ++ arrayType.all(func)
  }
}

case class ArraySize2(array: ReferenceExpression2) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Coord(Share, Int2()))
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ array.all(func)
  }
}

case class VoidLiteral2() extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Coord(Raw, Void2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class IntLiteral2(value: Int) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Coord(Share, Int2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class BoolLiteral2(value: Boolean) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Coord(Share, Bool2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class StrLiteral2(value: String) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Coord(Share, Str2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class FloatLiteral2(value: Float) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Coord(Share, Float2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class LocalLookup2(
    localVariable: ILocalVariable2,
    reference: Coord
) extends AddressExpression2 {
  override def resultRegister = AddressRegister2(reference)

  override def coerceToBorrow: Boolean = false

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ reference.all(func)
  }
}

case class ArgLookup2(
    paramIndex: Int,
    reference: Coord
) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(reference)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ reference.all(func)
  }
}

//case class PackLookup2(packExpr: Expression2, index: Int) extends Expression2 {
//  override def resultType: BaseType2 = {
//    // A pack can never be in a changeable variable, and so can't be an addressible, so will always
//    // be a pointer.
//    // (it can be in a final variable, when its spawned by pattern matching)
//    TypeUtils.softDecay(packExpr.resultType).innerType match {
//      case PackT2(memberTypes, underlyingStructRef) => memberTypes(index)
//    }
//  }
//
//  def all[T](func: PartialFunction[Ast2, T]): List[T] = {
//    List(this).collect(func) ++ packExpr.all(func)
//  }
//}

case class ArraySequenceLookup2(
    arrayExpr: ReferenceExpression2,
    arrayType: ArraySequenceT2,
    indexExpr: ReferenceExpression2) extends AddressExpression2 {
  vassert(arrayExpr.resultRegister.reference.referend == arrayType)

  override def resultRegister = AddressRegister2(arrayType.array.memberType)

  override def coerceToBorrow: Boolean = true

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ arrayExpr.all(func) ++ indexExpr.all(func) ++ arrayType.all(func)
  }
}

case class UnknownSizeArrayLookup2(
    arrayExpr: ReferenceExpression2,
    arrayType: UnknownSizeArrayT2,
    indexExpr: ReferenceExpression2) extends AddressExpression2 {
  vassert(arrayExpr.resultRegister.reference.referend == arrayType)

  override def resultRegister = AddressRegister2(arrayType.array.memberType)

  override def coerceToBorrow: Boolean = true

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ arrayExpr.all(func) ++ indexExpr.all(func) ++ arrayType.all(func)
  }
}

case class ArrayLength2(arrayExpr: ReferenceExpression2) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Coord(Share, Int2()))
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ arrayExpr.all(func)
  }
}

case class ReferenceMemberLookup2(
    structExpr: ReferenceExpression2,
    name: String,
    reference: Coord) extends AddressExpression2 {
  override def resultRegister = AddressRegister2(reference)

  override def coerceToBorrow: Boolean = true

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ structExpr.all(func) ++ reference.all(func)
  }
}
case class AddressMemberLookup2(
    structExpr: ReferenceExpression2,
    memberName: String,
    varId: VariableId2,
    resultType2: Coord) extends AddressExpression2 {
  override def resultRegister = AddressRegister2(resultType2)

  override def coerceToBorrow: Boolean = true

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ structExpr.all(func) ++ resultType2.all(func)
  }
}

case class FunctionLookup2(prototype: Prototype2) extends ReferenceExpression2 {
  override def resultRegister: ReferenceRegister2 =
    ReferenceRegister2(Coord(Raw, prototype.functionType))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ prototype.all(func)
  }
}
case class InterfaceFunctionCall2(
    superFunctionBanner: FunctionBanner2,
    functionType: Coord,
    resultReference: Coord,
    args: List[ReferenceExpression2]) extends ReferenceExpression2 {
  override def resultRegister: ReferenceRegister2 =
    ReferenceRegister2(resultReference)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ superFunctionBanner.all(func) ++ functionType.all(func) ++ resultReference.all(func) ++ args.flatMap(_.all(func))
  }
}

case class ExternFunctionCall2(
    prototype2: Prototype2,
    args: List[ReferenceExpression2]) extends ReferenceExpression2 {
  vassert(prototype2.fullName.steps.last.templateArgs.nonEmpty)
  vassert(prototype2.fullName.steps.last.templateArgs.get.isEmpty)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ args.flatMap(_.all(func))
  }

  override def resultRegister = ReferenceRegister2(prototype2.functionType.returnType)
}

// This should never be executed, it's just a placeholder that will be replaced later.
// For example, Array(4, {(i) i * 2 }) the ConstructArray2 contains a FunctionPointerCall
// which has the closure struct as the first argument, and the index as the second. But,
// the index isn't determined by an expression, it's determined by Vivem or Sculptor.
// So, we put a placeholder there, knowing it should be replaced later on.
case class Placeholder2(type2: Coord) extends ReferenceExpression2 {
  override def resultRegister: ReferenceRegister2 = {
    ReferenceRegister2(type2)
  }
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ type2.all(func)
  }
}

case class FunctionPointerCall2(
    callable: ReferenceExpression2,
    args: List[ReferenceExpression2]) extends ReferenceExpression2 {
  override def resultRegister: ReferenceRegister2 = {
    ReferenceRegister2(functionType.returnType)
  }
  def functionType: FunctionT2 = {
    callable.resultRegister.reference.referend match {
      case ft2 @ FunctionT2(_, returnType) => ft2
    }
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ callable.all(func) ++ args.flatMap(_.all(func))
  }
}
case class And2(
    left: ReferenceExpression2,
    right: ReferenceExpression2) extends ReferenceExpression2 {

  override def resultRegister = ReferenceRegister2(Coord(Share, Bool2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ left.all(func) ++ right.all(func)
  }
}

case class Tuple2(
    elements: List[ReferenceExpression2],
    tupleReference: Coord) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(tupleReference)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ elements.flatMap(_.all(func)) ++ tupleReference.all(func)
  }
}

// A templar reinterpret is interpreting a type as a different one which is hammer-equivalent.
// For example, a pack and a struct are the same thing to hammer.
// Also, a closure and a struct are the same thing to hammer.
// But, Templar attaches different meanings to these things. The templar is free to reinterpret
// between hammer-equivalent things as it wants.
case class TemplarReinterpret2(
    expr: ReferenceExpression2,
    resultReference: Coord) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(resultReference)

  // Unless it's a Never...
  if (expr.resultRegister.reference.referend != Never2()) {
    if (resultReference.ownership != expr.resultRegister.reference.ownership) {
      // Cant reinterpret to a different ownership!
      vfail("wat");
    }
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ expr.all(func) ++ resultReference.all(func)
  }
}

case class Construct2(
    structRef: StructRef2,
    resultReference: Coord,
    args: List[Expression2]) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(resultReference)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ structRef.all(func) ++ args.flatMap(_.all(func))
  }
}

// Note: the functionpointercall's last argument is a Placeholder2,
// it's up to later stages to replace that with an actual index
case class ConstructArray2(
    arrayType: UnknownSizeArrayT2,
    sizeExpr: ReferenceExpression2,
    call: FunctionPointerCall2) extends ReferenceExpression2 {
  override def resultRegister: ReferenceRegister2 = {
    ReferenceRegister2(
      Coord(
        if (arrayType.array.mutability == Mutable) Own else Share,
        arrayType))
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ arrayType.all(func) ++ sizeExpr.all(func) ++ call.all(func)
  }
}

// Note: the functionpointercall's last argument is a Placeholder2,
// it's up to later stages to replace that with an actual index
// This returns nothing, as opposed to DrainArraySequence2 which returns a
// sequence of results from the call.
case class DestroyArraySequence2(
    arrayExpr: ReferenceExpression2,
    arrayType: ArraySequenceT2,
    call: FunctionPointerCall2) extends ReferenceExpression2 {
  override def resultRegister: ReferenceRegister2 = ReferenceRegister2(Coord(Raw, Void2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ arrayType.all(func) ++ arrayExpr.all(func) ++ call.all(func)
  }
}

// Note: the functionpointercall's last argument is a Placeholder2,
// it's up to later stages to replace that with an actual index
case class DestroyUnknownSizeArray2(
    arrayExpr: ReferenceExpression2,
    arrayType: UnknownSizeArrayT2,
    call: FunctionPointerCall2) extends ReferenceExpression2 {
  override def resultRegister: ReferenceRegister2 = ReferenceRegister2(Coord(Raw, Void2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ arrayType.all(func) ++ arrayExpr.all(func) ++ call.all(func)
  }
}

// Note: the functionpointercall's last argument is a Placeholder2,
// it's up to later stages to replace that with an actual index
case class DrainArraySequence2(
    arrayExpr: ReferenceExpression2,
    arrayType: ArraySequenceT2,
    resultArraySeqType: ArraySequenceT2,
    resultArraySeqRefType: Coord,
    call: FunctionPointerCall2) extends ReferenceExpression2 {
  override def resultRegister: ReferenceRegister2 = ReferenceRegister2(resultArraySeqRefType)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ arrayType.all(func) ++ arrayExpr.all(func) ++ call.all(func)
  }
}

// Note: the functionpointercall's last argument is a Placeholder2,
// it's up to later stages to replace that with an actual index
case class DrainUnknownSizeArray2(
    arrayExpr: ReferenceExpression2,
    arrayType: UnknownSizeArrayT2,
    resultArraySeqType: UnknownSizeArrayT2,
    resultArraySeqRefType: Coord,
    call: FunctionPointerCall2) extends ReferenceExpression2 {
  override def resultRegister: ReferenceRegister2 = ReferenceRegister2(resultArraySeqRefType)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ arrayType.all(func) ++ arrayExpr.all(func) ++ call.all(func)
  }
}

case class InterfaceToInterfaceUpcast2(
    innerExpr: ReferenceExpression2,
    targetInterfaceRef: InterfaceRef2) extends ReferenceExpression2 {
  def resultRegister: ReferenceRegister2 = {
    ReferenceRegister2(
      Coord(
        innerExpr.resultRegister.reference.ownership,
        targetInterfaceRef))
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ innerExpr.all(func) ++ targetInterfaceRef.all(func)
  }
}

case class StructToInterfaceUpcast2(innerExpr: ReferenceExpression2, targetInterfaceRef: InterfaceRef2) extends ReferenceExpression2 {
  def resultRegister: ReferenceRegister2 = {
    ReferenceRegister2(
      Coord(
        innerExpr.resultRegister.reference.ownership,
        targetInterfaceRef))
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ innerExpr.all(func) ++ targetInterfaceRef.all(func)
  }
}

// A soft load is one that turns an int** into an int*. a hard load turns an int* into an int.
// Turns an Addressible(Pointer) into an OwningPointer. Makes the source owning pointer into null

// If the source was an own and target is borrow, that's a lend

case class SoftLoad2(expr: AddressExpression2, targetOwnership: Ownership) extends ReferenceExpression2 {

  vassert((targetOwnership == Share) == (expr.resultRegister.reference.ownership == Share))
  vassert((targetOwnership == Raw) == (expr.resultRegister.reference.ownership == Raw))

  override def resultRegister: ReferenceRegister2 = {
    ReferenceRegister2(Coord(targetOwnership, expr.resultRegister.reference.referend))
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ expr.all(func)
  }
}

// We destroy both Share and Own things
// If the struct contains any addressibles, those die immediately and aren't stored
// in the destination variables, which is why it's a list of ReferenceLocalVariable2.
case class Destructure2(
    expr: ReferenceExpression2,
    structRef2: StructRef2,
    destinationReferenceVariables: List[ReferenceLocalVariable2]
) extends ReferenceExpression2 {
  override def resultRegister: ReferenceRegister2 = ReferenceRegister2(Coord(Raw, Void2()))

  if (expr.resultRegister.reference.ownership == Borrow) {
    vfail("wot")
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ expr.all(func)
  }
}

//// If source was an own and target is borrow, that's a lend
//// (thats the main purpose of this)
//case class Alias2(expr: ReferenceExpression2, targetOwnership: Ownership) extends ReferenceExpression2 {
//  override def resultRegister: ReferenceRegister2 = {
//    expr.resultRegister.reference match {
//      case Coord(_, innerType) => ReferenceRegister2(Coord(targetOwnership, innerType))
//    }
//  }
//
//  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
//    List(this).collect(func) ++ expr.all(func)
//  }
//}

case class CheckRefCount2(
    refExpr: ReferenceExpression2,
    category: RefCountCategory,
    numExpr: ReferenceExpression2) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Coord(Raw, Void2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ refExpr.all(func) ++ numExpr.all(func)
  }
}
