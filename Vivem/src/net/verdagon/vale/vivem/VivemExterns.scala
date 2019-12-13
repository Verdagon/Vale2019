package net.verdagon.vale.vivem

import net.verdagon.vale.templar.types.{Raw, Share}
import net.verdagon.vale.vassert

object VivemExterns {
  def panic(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 0)
    throw new PanicException()
  }

  def addIntInt(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (IntV(aValue), IntV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, IntV(aValue + bValue)))
      }
    }
  }

  def addFloatFloat(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (FloatV(aValue), FloatV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, FloatV(aValue + bValue)))
      }
    }
  }

  def multiplyIntInt(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (IntV(aValue), IntV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, IntV(aValue * bValue)))
      }
    }
  }

  def multiplyFloatFloat(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (FloatV(aValue), FloatV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, FloatV(aValue * bValue)))
      }
    }
  }

  def mod(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (IntV(aValue), IntV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, IntV(aValue % bValue)))
      }
    }
  }

  def subtractIntInt(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (IntV(aValue), IntV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, IntV(aValue - bValue)))
      }
    }
  }

  def subtractFloatFloat(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (FloatV(aValue), FloatV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, FloatV(aValue - bValue)))
      }
    }
  }

  def addStrStr(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (StrV(aValue), StrV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, StrV(aValue + bValue)))
      }
    }
  }

  def getch(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.isEmpty)
    val next = memory.stdin()
    val code = if (next.isEmpty) { 0 } else { next.charAt(0).charValue().toInt }
    Some(memory.addAllocationForReturn(Share, IntV(code)))
  }

  def lessThanInt(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (IntV(aValue), IntV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, BoolV(aValue < bValue)))
      }
    }
  }

  def lessThanFloat(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (FloatV(aValue), FloatV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, BoolV(aValue < bValue)))
      }
    }
  }

  def greaterThanFloat(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (FloatV(aValue), FloatV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, BoolV(aValue > bValue)))
      }
    }
  }

  def lessThanOrEqInt(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (IntV(aValue), IntV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, BoolV(aValue <= bValue)))
      }
    }
  }

  def greaterThanInt(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (IntV(aValue), IntV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, BoolV(aValue > bValue)))
      }
    }
  }

  def greaterThanOrEqInt(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (IntV(aValue), IntV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, BoolV(aValue >= bValue)))
      }
    }
  }

  def eqIntInt(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (IntV(aValue), IntV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, BoolV(aValue == bValue)))
      }
    }
  }

  def eqBoolBool(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (BoolV(aValue), BoolV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, BoolV(aValue == bValue)))
      }
    }
  }

  def and(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (BoolV(aValue), BoolV(bValue)) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.dropReferenceIfNonOwning(args(1))
        Some(memory.addAllocationForReturn(Share, BoolV(aValue && bValue)))
      }
    }
  }

  def not(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 1)
    val BoolV(value) = memory.dereference(args(0))
    memory.dropReferenceIfNonOwning(args(0))
    Some(memory.addAllocationForReturn(Share, BoolV(!value)))
  }

  def sqrt(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 1)
    val FloatV(value) = memory.dereference(args(0))
    memory.dropReferenceIfNonOwning(args(0))
    Some(memory.addAllocationForReturn(Share, FloatV(Math.sqrt(value).toFloat)))
  }

  def castIntStr(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 1)
    val IntV(value) = memory.dereference(args(0))
    memory.dropReferenceIfNonOwning(args(0))
    Some(memory.addAllocationForReturn(Share, StrV(value.toString)))
  }

  def castFloatStr(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 1)
    val FloatV(value) = memory.dereference(args(0))
    memory.dropReferenceIfNonOwning(args(0))
    Some(memory.addAllocationForReturn(Share, StrV(value.toString)))
  }

  def castIntFloat(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 1)
    val IntV(value) = memory.dereference(args(0))
    memory.dropReferenceIfNonOwning(args(0))
    Some(memory.addAllocationForReturn(Share, FloatV(value.toFloat)))
  }

  def print(memory: AdapterForExterns, args: Vector[ReferenceV]): Option[ReturnV] = {
    vassert(args.size == 1)
    memory.dereference(args(0)) match {
      case StrV(value) => {
        memory.dropReferenceIfNonOwning(args(0))
        memory.stdout(value)
      }
    }
    None
  }
}