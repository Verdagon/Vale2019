package net.verdagon.vale.metal

import net.verdagon.vale.{vassert, vcurious, vfail}

// Common trait for all instructions.
sealed trait NodeH {
  // The resulting registerId produced by this instruction.
  def registerId: String;
}

// Creates an integer and puts it into a register.
case class ConstantI64H(
  // The register ID to put the integer into.
  registerId: String,
  // The value of the integer.
  value: Int
) extends NodeH

// Creates a Void, which can be thought of as an empty struct, and puts it into a register.
// TODO: See if we can replace this with just an empty struct (inlined), to simplify things.
case class ConstantVoidH(
  // The register ID to put the void into.
  registerId: String
) extends NodeH

// Creates a boolean and puts it into a register.
case class ConstantBoolH(
  // The register ID to put the boolean into.
  registerId: String,
  // The value of the boolean.
  value: Boolean
) extends NodeH

// Creates a string and puts it into a register.
case class ConstantStrH(
  // The register ID to put the string into.
  registerId: String,
  // The value of the string.
  value: String
) extends NodeH

// Creates a float and puts it into a register.
case class ConstantF64H(
  // The register ID to put the float into.
  registerId: String,
  // The value of the float.
  value: Float
) extends NodeH

// Grabs the argument and puts it into a register.
// There can only be one of these per argument; this conceptually destroys
// the containing argument and puts its value into the register.
case class ArgumentH(
  // The register ID to put the argument's value into.
  registerId: String,
  // The argument's type.
  resultReference: ReferenceH[ReferendH],
  // The index of the argument, starting at 0.
  argumentIndex: Int
) extends NodeH

// Takes a value from a register (the "source" register) and puts it into a local
// variable on the stack.
case class StackifyH(
  // Unused, ignore this. It's just here to conform to the NodeH trait.
  registerId: String,
  // The register to read a value from.
  // As with any read from a register, this will invalidate the register.
  sourceRegister: RegisterAccessH[ReferendH],
  // Describes the local we're making.
  local: Local,
  // Name of the local variable. Unique per block.
  name: String
) extends NodeH

// Takes a value from a local variable on the stack, and moves it into a register.
// The local variable is now invalid, since its value has been taken out.
// See LocalLoadH for a similar instruction that *doesnt* invalidate the local var.
case class UnstackifyH(
  // The register ID to put the local's value into.
  registerId: String,
  // Describes the local we're pulling from. This is equal to the corresponding
  // StackifyH's `local` member.
  local: Local,
  // The type we expect to get out of the local. Should be equal to local.
  // TODO: If the vcurious below doesn't panic, then let's get rid of this redundant member.
  expectedType: ReferenceH[ReferendH]
) extends NodeH {
  // Panics if this is ever not the case.
  vcurious(local.typeH == expectedType)
}

// Takes a struct from the given register, and destroys it.
// All of its members are saved from the jaws of death, and put into the specified
// local variables.
// This creates those local variables, much as a StackifyH would, and puts into them
// the values from the dying struct instance.
case class DestructureH(
  // Unused, ignore. Just here to conform to the NodeH trait.
  registerId: String,
  // The register to take the struct from.
  // As with any read from a register, this will invalidate the register.
  structRegister: RegisterAccessH[StructRefH],
  // A list of types, one for each local variable we'll make.
  // TODO: If the vcurious below doesn't panic, get rid of this redundant member.
  localTypes: List[ReferenceH[ReferendH]],
  // The locals to put the struct's members into.
  localIndices: Vector[Local]
) extends NodeH {
  vassert(localTypes.size == localIndices.size)
  vcurious(localTypes == localIndices.map(_.typeH).toList)
}

// Takes a struct reference from the "source" register, and makes an interface reference
// to it, as the "target" reference, and puts it into another register.
case class StructToInterfaceUpcastH(
  // The register ID to put the interface reference into.
  registerId: String,
  // The register to get the struct reference from.
  // As with any read from a register, this will invalidate the register.
  sourceRegister: RegisterAccessH[StructRefH],
  // The type of interface to cast to.
  targetInterfaceRef: InterfaceRefH
) extends NodeH {
  // The resulting type will have the same ownership as the source register had.
  def resultRef = ReferenceH(sourceRegister.expectedType.ownership, targetInterfaceRef)
}

// Takes an interface reference from the "source" register, and makes another reference
// to it, as the "target" inference, and puts it into another register.
case class InterfaceToInterfaceUpcastH(
  // The register ID to put the target interface reference into.
  registerId: String,
  // The register to get the source interface reference from.
  // As with any read from a register, this will invalidate the register.
  sourceRegister: RegisterAccessH[InterfaceRefH],
  // The type of interface to cast to.
  targetInterfaceRef: InterfaceRefH
) extends NodeH {
  // The resulting type will have the same ownership as the source register had.
  def resultRef = ReferenceH(sourceRegister.expectedType.ownership, targetInterfaceRef)
}

case class UnreachableH(registerId: String) extends NodeH {
  def resultType = ReferenceH(Share, NeverH())
}

// Takes a reference from the given "source" register, and puts it into an *existing*
// local variable.
case class LocalStoreH(
  // Unused, ignore. Only here to conform to the NodeH trait.
  registerId: String,
  // The existing local to store into.
  local: Local,
  // The register to get the source reference from.
  // As with any read from a register, this will invalidate the register.
  sourceRegister: RegisterAccessH[ReferendH],
  // Name of the local variable, for debug purposes.
  localName: String
) extends NodeH

// Takes a reference from the given local variable, and puts it into a new register.
// This can never move a reference, only alias it. The instruction which can move a
// reference is UnstackifyH.
case class LocalLoadH(
  // The register ID to put the target interface reference into.
  registerId: String,
  // The existing local to load from.
  local: Local,
  // The ownership of the reference to put into the register. This doesn't have to
  // match the ownership of the reference from the local. For example, we might want
  // to load a constraint reference from an owning local.
  targetOwnership: Ownership,
  // TODO: Get rid of this, it's reduntant.
  expectedLocalType: ReferenceH[ReferendH],
  // TODO: Get rid of this, it's reduntant.
  expectedResultType: ReferenceH[ReferendH],
  // Name of the local variable, for debug purposes.
  localName: String
) extends NodeH {
  vassert(expectedLocalType.kind == expectedResultType.kind)
  vassert(expectedResultType.ownership == targetOwnership)
}

// Takes a reference from the given "source" register, and swaps it into the given
// struct's member. The member's old reference is put into a new register.
case class MemberStoreH(
  // The register ID to put the member's old value into.
  registerId: String,
  // Register containing a reference to the struct whose member we will swap.
  structRegister: RegisterAccessH[StructRefH],
  // Which member to swap out, starting at 0.
  memberIndex: Int,
  // Register containing the new value for the struct's member.
  // As with any read from a register, this will invalidate the register.
  sourceRegister: RegisterAccessH[ReferendH],
  // Name of the member, for debug purposes.
  memberName: String
) extends NodeH

// Takes a reference from the given "struct" register, and copies it into a new
// register. This can never move a reference, only alias it.
case class MemberLoadH(
  // The register ID to put the member's value into.
  registerId: String,
  // Register containing a reference to the struct whose member we will read.
  // As with any read from a register, this will invalidate the register.
  structRegister: RegisterAccessH[StructRefH],
  // Which member to read from, starting at 0.
  memberIndex: Int,
  // The ownership to load as. For example, we might load a constraint reference from a
  // owning Car reference member.
  targetOwnership: Ownership,
  // The type we expect the member to be. This can easily be looked up, but is provided
  // here to be convenient for LLVM.
  expectedMemberType: ReferenceH[ReferendH],
  // The type of the resulting reference.
  expectedResultType: ReferenceH[ReferendH],
  // Member's name, for debug purposes.
  memberName: String
) extends NodeH {
  vassert(expectedMemberType.kind == expectedResultType.kind)
  vassert(expectedResultType.ownership == targetOwnership)
}

// Creates an array whose size is fixed and known at compile time, and puts it into
// a register.
case class NewArrayFromValuesH(
  // The register to put the array into.
  registerId: String,
  // The registers from which we'll get the values to put into the array.
  // As with any read from a register, this will invalidate the registers.
  sourceRegisters: List[RegisterAccessH[ReferendH]],
  // The resulting type of the array.
  // TODO: See if we can infer this from the types in the registers.
  arrayRefType: ReferenceH[KnownSizeArrayTH]
) extends NodeH

// Loads from the "source" register and swaps it into the array from arrayRegister at
// the position specified by the integer in indexRegister. The old value from the
// array is moved out into registerId.
// This is for the kind of array whose size we know at compile time, the kind that
// doesn't need to carry around a size. For the corresponding instruction for the
// unknown-size-at-compile-time array, see UnknownSizeArrayStoreH.
case class KnownSizeArrayStoreH(
  // The register to store the old value in.
  registerId: String,
  // Register containing the array whose element we'll swap out.
  // As with any read from a register, this will invalidate the register.
  arrayRegister: RegisterAccessH[KnownSizeArrayTH],
  // Register containing the index of the element we'll swap out.
  // As with any read from a register, this will invalidate the register.
  indexRegister: RegisterAccessH[IntH],
  // Register containing the value we'll swap into the array.
  // As with any read from a register, this will invalidate the register.
  sourceRegister: RegisterAccessH[ReferendH]
) extends NodeH

// Loads from the "source" register and swaps it into the array from arrayRegister at
// the position specified by the integer in indexRegister. The old value from the
// array is moved out into registerId.
// This is for the kind of array whose size we don't know at compile time, the kind
// that needs to carry around a size. For the corresponding instruction for the
// known-size-at-compile-time array, see KnownSizeArrayStoreH.
case class UnknownSizeArrayStoreH(
  // The register to store the old value in.
  registerId: String,
  // Register containing the array whose element we'll swap out.
  // As with any read from a register, this will invalidate the register.
  arrayRegister: RegisterAccessH[UnknownSizeArrayTH],
  // Register containing the index of the element we'll swap out.
  // As with any read from a register, this will invalidate the register.
  indexRegister: RegisterAccessH[IntH],
  // Register containing the value we'll swap into the array.
  // As with any read from a register, this will invalidate the register.
  sourceRegister: RegisterAccessH[ReferendH]
) extends NodeH

// Loads from the array in arrayRegister at the index in indexRegister, and stores
// the result in registerId. This can never move a reference, only alias it.
// This is for the kind of array whose size we don't know at compile time, the kind
// that needs to carry around a size. For the corresponding instruction for the
// known-size-at-compile-time array, see KnownSizeArrayLoadH.
case class UnknownSizeArrayLoadH(
  // The register to store the value in.
  registerId: String,
  // Register containing the array whose element we'll read.
  // As with any read from a register, this will invalidate the register.
  arrayRegister: RegisterAccessH[UnknownSizeArrayTH],
  // Register containing the index of the element we'll read.
  // As with any read from a register, this will invalidate the register.
  indexRegister: RegisterAccessH[IntH],
  // Resulting reference's type.
  // TODO: Remove this, it's redundant with targetOwnership and the array's element type.
  resultType: ReferenceH[ReferendH],
  // The ownership to load as. For example, we might load a constraint reference from a
  // owning Car reference element.
  targetOwnership: Ownership
) extends NodeH

// Loads from the array in arrayRegister at the index in indexRegister, and stores
// the result in registerId. This can never move a reference, only alias it.
// This is for the kind of array whose size we know at compile time, the kind that
// doesn't need to carry around a size. For the corresponding instruction for the
// known-size-at-compile-time array, see KnownSizeArrayStoreH.
case class KnownSizeArrayLoadH(
  // The register to store the value in.
  registerId: String,
  // Register containing the array whose element we'll read.
  // As with any read from a register, this will invalidate the register.
  arrayRegister: RegisterAccessH[KnownSizeArrayTH],
  // Register containing the index of the element we'll read.
  // As with any read from a register, this will invalidate the register.
  indexRegister: RegisterAccessH[IntH],
  // Resulting reference's type.
  // TODO: Remove this, it's redundant with targetOwnership and the array's element type.
  resultType: ReferenceH[ReferendH],
  // The ownership to load as. For example, we might load a constraint reference from a
  // owning Car reference element.
  targetOwnership: Ownership
) extends NodeH

// Calls a function.
case class CallH(
  // The register to put the returned value into.
  registerId: String,
  // Identifies which function to call.
  function: PrototypeH,
  // Registers containing the arguments to pass to the function.
  // As with any read from a register, this will invalidate the registers.
  argsRegisters: List[RegisterAccessH[ReferendH]]
) extends NodeH

// Calls a function on an interface.
case class InterfaceCallH(
  // The register to put the returned value into.
  registerId: String,
  // Registers containing the arguments to pass to the function.
  // As with any read from a register, this will invalidate the registers.
  argsRegisters: List[RegisterAccessH[ReferendH]],
  // Which parameter has the interface whose table we'll read to get the function.
  virtualParamIndex: Int,
  // The type of the interface.
  // TODO: Take this out, it's redundant, can get it from argsRegisters[virtualParamIndex]
  interfaceRefH: InterfaceRefH,
  // The index in the vtable for the function.
  indexInEdge: Int,
  // The function we expect to be calling. Note that this is the prototype for the abstract
  // function, not the prototype for the function that will eventually be called. The
  // difference is that this prototype will have an interface at the virtualParamIndex'th
  // parameter, and the function that is eventually called will have a struct there.
  functionType: PrototypeH
) extends NodeH {
  vassert(indexInEdge >= 0)
}

// An if-statement. It will get a boolean from running conditionBlock, and use it to either
// call thenBlock or elseBlock. The result of the thenBlock or elseBlock will be put into
// registerId.
case class IfH(
  // The register we'll put the then/else's result into.
  registerId: String,
  // The block for the condition. If this results in a true, we'll run thenBlock, otherwise
  // we'll run elseBlock.
  conditionBlock: BlockH,
  // The block to run if conditionBlock results in a true. The result of this block will be
  // put into registerId.
  thenBlock: BlockH,
  // The block to run if conditionBlock results in a false. The result of this block will be
  // put into registerId.
  elseBlock: BlockH
) extends NodeH

// A while loop. Continuously runs bodyBlock until it returns false.
case class WhileH(
  // Unused, ignore. It's just here to conform to the NodeH trait.
  registerId: String,
  // The block to run until it returns false.
  bodyBlock: BlockH
) extends NodeH

// TODO: Get rid of this, merge it into BlockH.
case class InlineBlockH(
  registerId: String,
  block: BlockH
) extends NodeH

// A collection of instructions. The last one will be used as the block's result.
case class BlockH(
  // The instructions to run.
  nodes: Vector[NodeH],
  // The resulting type of the block.
  // TODO: Get rid of this, it's redundant with the last node's result type.
  resultType: ReferenceH[ReferendH]
) {
  vassert(nodes.nonEmpty)
  if (nodes.map(_.registerId).toSet.size != nodes.size) {
    vfail("wat")
  }
}

// Ends the current function and returns a reference. A function will always end
// with a return statement.
case class ReturnH(
  // Unused, ignore. Only here to conform to the NodeH trait.
  registerId: String,
  // The register to read from, whose value we'll return from the function.
  // As with any read from a register, this will invalidate the register.
  sourceRegister: RegisterAccessH[ReferendH]
) extends NodeH

// Constructs an unknown-size array, whose length is the integer from sizeRegister,
// whose values are generated from the function from generatorRegister. Puts the
// result in a new register.
case class ConstructUnknownSizeArrayH(
  // The register we'll put the new array into.
  registerId: String,
  // Register containing the size of the new array.
  // As with any read from a register, this will invalidate the register.
  sizeRegister: RegisterAccessH[IntH],
  // Register containing the IFunction<Int, T> interface reference which we'll
  // call to generate each element of the array.
  // More specifically, we'll call the "__call" function on the interface, which
  // should be the only function on it.
  // This is a constraint reference.
  // As with any read from a register, this will invalidate the register.
  generatorRegister: RegisterAccessH[InterfaceRefH],
  // The resulting type of the array.
  // TODO: Remove this, it's redundant with the generatorRegister's interface's
  // only method's return type.
  arrayRefType: ReferenceH[UnknownSizeArrayTH]
) extends NodeH {
  vassert(generatorRegister.expectedType.ownership == Borrow)
}

// Destroys an array previously created with NewArrayFromValuesH.
case class DestroyKnownSizeArrayH(
  // Unused, ignore. Just here to conform to the NodeH trait.
  registerId: String,
  // Register containing the array we'll destroy.
  // This is an owning reference.
  // As with any read from a register, this will invalidate the register.
  arrayRegister: RegisterAccessH[KnownSizeArrayTH],
  // Register containing the IFunction<T, Void> interface reference which we'll
  // call to destroy each element of the array.
  // More specifically, we'll call the "__call" function on the interface, which
  // should be the only function on it.
  // This is a constraint reference.
  // As with any read from a register, this will invalidate the register.
  consumerRegister: RegisterAccessH[InterfaceRefH]
) extends NodeH

// Destroys an array previously created with ConstructUnknownSizeArrayH.
case class DestroyUnknownSizeArrayH(
  // Unused, ignore. Just here to conform to the NodeH trait.
  registerId: String,
  // Register containing the array we'll destroy.
  // This is an owning reference.
  // As with any read from a register, this will invalidate the register.
  arrayRegister: RegisterAccessH[UnknownSizeArrayTH],
  // Register containing the IFunction<T, Void> interface reference which we'll
  // call to destroy each element of the array.
  // More specifically, we'll call the "__call" function on the interface, which
  // should be the only function on it.
  // This is a constraint reference.
  // As with any read from a register, this will invalidate the register.
  consumerRegister: RegisterAccessH[InterfaceRefH]
) extends NodeH

// Creates a new struct instance.
case class NewStructH(
  // The register we'll put the reference into.
  // This should be an owning reference (or shared, if the struct's immutable).
  registerId: String,
  // Registers containing the values we'll use as members of the new struct.
  // As with any read from a register, this will invalidate the register.
  sourceRegisters: List[RegisterAccessH[ReferendH]],
  // The type of struct we'll create.
  structRefType: ReferenceH[StructRefH]
) extends NodeH

// Gets the length of an unknown-sized array.
case class ArrayLengthH(
  // The register we'll put the length into.
  registerId: String,
  // Register containing the array whose length we'll get.
  // As with any read from a register, this will invalidate the register.
  sourceRegister: RegisterAccessH[ReferendH],
) extends NodeH

// Only used for the VM, or a debug mode. Checks that the reference count
// is as we expected.
// This instruction can be safely ignored, it's mainly here for tests.
case class CheckRefCountH(
  // Unused, ignore. Just here to conform to the NodeH trait.
  registerId: String,
  // Register containing the reference whose ref count we'll measure.
  refRegister: RegisterAccessH[ReferendH],
  // The type of ref count to check.
  category: RefCountCategory,
  // Register containing a number, so we can assert it's equal to the object's
  // ref count.
  numRegister: RegisterAccessH[IntH]
) extends NodeH
// The type of ref count that an object might have. Used with the CheckRefCountH
// instruction for counting how many references of a certain type there are.
sealed trait RefCountCategory
// Used to count how many variables are refering to an object.
case object VariableRefCount extends RefCountCategory
// Used to count how many members are refering to an object.
case object MemberRefCount extends RefCountCategory
// Used to count how many registers are refering to an object.
case object RegisterRefCount extends RefCountCategory

// Discards a register, making it invalid.
// This instruction can be safely ignored. It's used by the VM to ensure that
// we consistently discard all registers that were created, in the exact reverse
// order they were created.
case class DiscardH(
  registerId: String,
  sourceRegister: RegisterAccessH[ReferendH]
) extends NodeH

// A convenience class that represents reading from a register. Contains the register ID
// and the type we expect it to contain.
// Remember that reading from a register will invalidate the register.
case class RegisterAccessH[+T <: ReferendH](
  registerId: String,
  expectedType: ReferenceH[T]) {

  vassert(expectedType.kind != VoidH())

  def expectStructAccess(): RegisterAccessH[StructRefH] = {
    this match {
      case RegisterAccessH(registerId, ReferenceH(ownership, x @ StructRefH(_))) => {
        RegisterAccessH[StructRefH](registerId, ReferenceH(ownership, x))
      }
    }
  }
  def expectInterfaceAccess(): RegisterAccessH[InterfaceRefH] = {
    this match {
      case RegisterAccessH(registerId, ReferenceH(ownership, x @ InterfaceRefH(_))) => {
        RegisterAccessH[InterfaceRefH](registerId, ReferenceH(ownership, x))
      }
    }
  }
  def expectUnknownSizeArrayAccess(): RegisterAccessH[UnknownSizeArrayTH] = {
    this match {
      case RegisterAccessH(registerId, ReferenceH(ownership, x @ UnknownSizeArrayTH(_))) => {
        RegisterAccessH[UnknownSizeArrayTH](registerId, ReferenceH(ownership, x))
      }
    }
  }
  def expectKnownSizeArrayAccess(): RegisterAccessH[KnownSizeArrayTH] = {
    this match {
      case RegisterAccessH(registerId, ReferenceH(ownership, x @ KnownSizeArrayTH(_, _))) => {
        RegisterAccessH[KnownSizeArrayTH](registerId, ReferenceH(ownership, x))
      }
    }
  }
  def expectIntAccess(): RegisterAccessH[IntH] = {
    this match {
      case RegisterAccessH(registerId, ReferenceH(ownership, x @ IntH())) => {
        RegisterAccessH[IntH](registerId, ReferenceH(ownership, x))
      }
    }
  }
  def expectBoolAccess(): RegisterAccessH[BoolH] = {
    this match {
      case RegisterAccessH(registerId, ReferenceH(ownership, x @ BoolH())) => {
        RegisterAccessH[BoolH](registerId, ReferenceH(ownership, x))
      }
    }
  }
}

trait IRegisterH {
  def expectReferenceRegister(): ReferenceRegisterH = {
    this match {
      case r @ ReferenceRegisterH(_) => r
      case AddressRegisterH(_) => vfail("Expected a reference as a result, but got an address!")
    }
  }
  def expectAddressRegister(): AddressRegisterH = {
    this match {
      case a @ AddressRegisterH(_) => a
      case ReferenceRegisterH(_) => vfail("Expected an address as a result, but got a reference!")
    }
  }
}
case class ReferenceRegisterH(reference: ReferenceH[ReferendH]) extends IRegisterH
case class AddressRegisterH(reference: ReferenceH[ReferendH]) extends IRegisterH

// Identifies a local variable.
case class Local(
  // No two variables in a FunctionH have the same id.
  id: VariableIdH,

  // Multiple variables in a FunctionH can have the same height. For example:
  // fn main() {
  //   {
  //     x = 4;
  //   }
  //   {
  //     y = 4;
  //   }
  // }
  // Both of these will have index 0.
  // In the context of JVM, this is the local index.
  // In LLVM, this could almost be thought of as where it is on the stack.
  height: StackHeight,

  // The type of the reference this local variable has.
  typeH: ReferenceH[ReferendH])

case class VariableIdH(
  // Just to uniquify VariableIdH instances. No two variables in a FunctionH will have
  // the same number.
  number: Int,
  // Just for debugging purposes
  name: Option[String])

case class StackHeightBox(var inner: StackHeight) {
  def blockHeight: Int = inner.blockHeight
  def blockStartLocalsHeight: Int = inner.blockStartLocalsHeight
  def localsHeight: Int = inner.localsHeight
  def snapshot = inner

  def oneLocalHigher(): Unit = {
    inner = inner.oneLocalHigher()
  }
  def oneBlockHigher(): Unit = {
    inner = inner.oneBlockHigher()
  }
}

case class StackHeight(
  blockHeight: Int, // How many blocks deep we are in the function. The first block is 0
  blockStartLocalsHeight: Int, // At the start of the block, how many locals are on the stack
  localsHeight: Int, // How many locals are on the stack right now total for this function
) {
  def oneLocalHigher() = {
    StackHeight(blockHeight, blockStartLocalsHeight, localsHeight + 1)
  }
  def oneBlockHigher() = {
    StackHeight(blockHeight + 1, localsHeight, localsHeight)
  }
}
