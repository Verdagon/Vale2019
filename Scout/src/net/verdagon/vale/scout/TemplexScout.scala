package net.verdagon.vale.scout

import net.verdagon.vale.parser._

object TemplexScout {
  def translateMaybeTemplex(declaredRunes: Set[String], maybeTemplexP: Option[ITemplexPT]): Option[ITemplexS] = {
    maybeTemplexP match {
      case None => None
      case Some(t) => Some(translateTemplex(declaredRunes, t))
    }
  }

  def translateTemplex(declaredRunes: Set[String], templexP: ITemplexPT): ITemplexS = {
    templexP match {
      case NameOrRunePT(nameOrRune) => {
        if (declaredRunes.contains(nameOrRune)) {
          NameST(nameOrRune)
        } else {
          RuneST(nameOrRune)
        }
      }
      case MutabilityPT(mutability) => MutabilityST(mutability)
      case CallPT(template, args) => CallST(translateTemplex(declaredRunes, template), args.map(arg => translateTemplex(declaredRunes, arg)))
      case NullablePT(inner) => NullableST(translateTemplex(declaredRunes, inner))
      case BorrowPT(inner) => OwnershippedST(BorrowP, translateTemplex(declaredRunes, inner))
      case SharePT(inner) => OwnershippedST(ShareP, translateTemplex(declaredRunes, inner))
      case OwnPT(inner) => OwnershippedST(OwnP, translateTemplex(declaredRunes, inner))
      case ArraySequencePT(mutability, size, element) => RepeaterSequenceST(translateTemplex(declaredRunes, mutability), translateTemplex(declaredRunes, size), translateTemplex(declaredRunes, element))
    }
  }
}
