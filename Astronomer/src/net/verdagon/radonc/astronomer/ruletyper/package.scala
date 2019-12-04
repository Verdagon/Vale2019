package net.verdagon.radonc.astronomer

import net.verdagon.radonc.vassert

package object ruletyper {
  case class Conclusions(
      typeByRune: Map[String, ITemplataType]) {
    def addConclusion(rune: String, tyype: ITemplataType): Conclusions = {
      vassert(!typeByRune.contains(rune))
      Conclusions(typeByRune + (rune -> tyype))
    }
  }

  trait IConflictCause
  case class MultipleCauses(causes: List[IConflictCause])

  sealed trait IRuleTyperSolveResult[T]
  case class RuleTyperSolveFailure[T](
    conclusions: Conclusions,
    message: String,
    inner: List[IConflictCause]
  ) extends IRuleTyperSolveResult[T] with IConflictCause
  case class RuleTyperSolveSuccess[T](
    conclusions: Conclusions,
    types: T
  ) extends IRuleTyperSolveResult[T]


  sealed trait IRuleTyperEvaluateResult[+T]
  case class RuleTyperEvaluateConflict[T](
    // This is in here because when we do an Or rule, we want to know why each
    // case failed; we want to have all the conflicts in a row, we want to have
    // the conclusions for each failure.
    conclusions: Conclusions,
    message: String,
    cause: Option[IConflictCause]
  ) extends IRuleTyperEvaluateResult[T] with IConflictCause
  case class RuleTyperEvaluateUnknown[T](
    conclusions: Conclusions,
  ) extends IRuleTyperEvaluateResult[T]
  case class RuleTyperEvaluateSuccess[+T](
    conclusions: Conclusions,
    result: T
  ) extends IRuleTyperEvaluateResult[T]


  sealed trait IRuleTyperMatchResult[+T]
  case class RuleTyperMatchConflict[+T](
    // This is in here because when we do an Or rule, we want to know why each
    // case failed; we want to have all the conflicts in a row, we want to have
    // the conclusions for each failure.
    conclusions: Conclusions,
    message: String,
    // For an Or rule, this will contain all the conflicts for each branch.
    causes: List[IConflictCause]
  ) extends IRuleTyperMatchResult[T] with IConflictCause {
    override def toString: String = {
      // The # signals the reader that we overrode toString
      "RuleTyperMatchConflict#(" + message + ", " + causes + ", " + conclusions + ")"
    }
  }
  case class RuleTyperMatchUnknown[+T](
    conclusions: Conclusions,
  ) extends IRuleTyperMatchResult[T]
  case class RuleTyperMatchSuccess[+T](
    conclusions: Conclusions,
    result: T
  ) extends IRuleTyperMatchResult[T]
}
