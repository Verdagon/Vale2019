package net.verdagon.vale.scout

import net.verdagon.vale.parser._

object TemplexScout {
  def translateMaybeTemplex(maybeTemplexP: Option[ITemplexPT]): Option[ITemplexS] = {
    maybeTemplexP match {
      case None => None
      case Some(t) => Some(translateTemplex(t))
    }
  }

  def translateTemplex(templexP: ITemplexPT): ITemplexS = {
    templexP match {
      case NamePT(name) => NameST(name)
      case RunePT(rune) => RuneST(rune)
      case MutabilityPT(mutability) => MutabilityST(mutability)
      case CallPT(template, args) => CallST(translateTemplex(template), args.map(arg => translateTemplex(arg)))
      case NullablePT(inner) => NullableST(translateTemplex(inner))
      case BorrowPT(inner) => OwnershippedST(BorrowP, translateTemplex(inner))
      case SharePT(inner) => OwnershippedST(ShareP, translateTemplex(inner))
      case OwnPT(inner) => OwnershippedST(OwnP, translateTemplex(inner))
      case ArraySequencePT(mutability, size, element) => RepeaterSequenceST(translateTemplex(mutability), translateTemplex(size), translateTemplex(element))
    }
  }
}
