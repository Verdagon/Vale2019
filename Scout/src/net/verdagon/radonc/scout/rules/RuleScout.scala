package net.verdagon.radonc.scout.rules

import net.verdagon.radonc.parser._
import net.verdagon.radonc.scout._

object RuleScout {
  def translateRulexes(rules: List[IRulexPR]): List[IRulexSR] = {
    rules.map(translateRulex)
  }
  def translateRulex(rulex: IRulexPR): IRulexSR = {
    rulex match {
      case EqualsPR(left, right) => EqualsSR(translateRulex(left), translateRulex(right))
      case OrPR(possibilities) => OrSR(possibilities.map(translateRulex))
      case ComponentsPR(TypedPR(maybeRune, tyype), components) => {
        ComponentsSR(TypedSR(maybeRune, translateType(tyype)), components.map(translateRulex))
      }
      case TypedPR(rune, tyype) => TypedSR(rune, translateType(tyype))
      case TemplexPR(templex) => TemplexSR(translateTemplex(templex))
      case CallPR(name, args) => CallSR(name, args.map(translateRulex))

    }
  }
  def translateType(tyype: ITypePR): ITypeSR = {
    tyype match {
      case IntTypePR => IntTypeSR
      case BoolTypePR => BoolTypeSR
      case OwnershipTypePR => OwnershipTypeSR
      case MutabilityTypePR => MutabilityTypeSR
      case PermissionTypePR => PermissionTypeSR
      case LocationTypePR => LocationTypeSR
      case CoordTypePR => CoordTypeSR
      case KindTypePR => KindTypeSR
//      case StructTypePR => KindTypeSR
//      case SequenceTypePR => KindTypeSR
//      case ArrayTypePR => KindTypeSR
//      case CallableTypePR => KindTypeSR
//      case InterfaceTypePR => KindTypeSR
    }
  }
  def translateMaybeTemplex(maybeTemplex: Option[ITemplexPRT]): ITemplexS = {
    maybeTemplex.map(translateTemplex).getOrElse(AnonymousRuneST())
  }

  def translateTemplex(templex: ITemplexPRT): ITemplexS = {
    templex match {
      case IntPRT(value) => IntST(value)
      case MutabilityPRT(mutability) => MutabilityST(mutability)
      case PermissionPRT(permission) => PermissionST(permission)
      case LocationPRT(location) => LocationST(location)
      case OwnershipPRT(ownership) => OwnershipST(ownership)
      case VariabilityPRT(variability) => VariabilityST(variability)
      case BoolPRT(value) => BoolST(value)
      case NamePRT(name) => NameST(name)
      case RunePRT(rune) => RuneST(rune)
      case AnonymousRunePRT() => AnonymousRuneST()
      case CallPRT(template, args) => CallST(translateTemplex(template), args.map(translateTemplex))
      case FunctionPRT(mutability, paramsPack, returnType) => {
        CallST(
          NameST("IFunction"),
          List(
            mutability match { case None => MutabilityST(MutableP) case Some(m) => translateTemplex(m) },
            translateTemplex(paramsPack),
            translateTemplex(returnType)))
      }
      case PrototypePRT(name, parameters, returnType) => PrototypeST(name, parameters.map(translateTemplex), translateTemplex(returnType))
      case PackPRT(members) => PackST(members.map(translateTemplex))
      case RepeaterSequencePRT(mutability, size, element) => RepeaterSequenceST(translateTemplex(mutability), translateTemplex(size), translateTemplex(element))
      case ManualSequencePRT(elements) => ManualSequenceST(elements.map(translateTemplex))
    }
  }
}
