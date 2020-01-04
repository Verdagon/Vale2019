package net.verdagon.vale.scout.rules

import net.verdagon.vale.parser._
import net.verdagon.vale.scout.{IEnvironment => _, FunctionEnvironment => _, Environment => _, _}

object RuleScout {
  def translateRulexes(userDeclaredRunes: Set[String], rules: List[IRulexPR]): List[IRulexSR] = {
    rules.map(translateRulex(userDeclaredRunes, _))
  }
  def translateRulex(userDeclaredRunes: Set[String], rulex: IRulexPR): IRulexSR = {
    rulex match {
      case EqualsPR(left, right) => EqualsSR(translateRulex(userDeclaredRunes, left), translateRulex(userDeclaredRunes, right))
      case OrPR(possibilities) => OrSR(possibilities.map(translateRulex(userDeclaredRunes, _)))
      case ComponentsPR(TypedPR(maybeRune, tyype), components) => {
        ComponentsSR(TypedSR(maybeRune, translateType(tyype)), components.map(translateRulex(userDeclaredRunes, _)))
      }
      case TypedPR(rune, tyype) => TypedSR(rune, translateType(tyype))
      case TemplexPR(templex) => TemplexSR(translateTemplex(userDeclaredRunes, templex))
      case CallPR(name, args) => CallSR(name, args.map(translateRulex(userDeclaredRunes, _)))
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
  def translateMaybeTemplex(userDeclaredRunes: Set[String], maybeTemplex: Option[ITemplexPRT]): ITemplexS = {
    maybeTemplex.map(translateTemplex(userDeclaredRunes, _)).getOrElse(AnonymousRuneST())
  }

  def translateTemplex(userDeclaredRunes: Set[String], templex: ITemplexPRT): ITemplexS = {
    templex match {
      case IntPRT(value) => IntST(value)
      case MutabilityPRT(mutability) => MutabilityST(mutability)
      case PermissionPRT(permission) => PermissionST(permission)
      case LocationPRT(location) => LocationST(location)
      case OwnershipPRT(ownership) => OwnershipST(ownership)
      case VariabilityPRT(variability) => VariabilityST(variability)
      case BoolPRT(value) => BoolST(value)
      case NameOrRunePRT(name) => if (userDeclaredRunes.contains(name)) RuneST(name) else NameST(name)
      case AnonymousRunePRT() => AnonymousRuneST()
      case CallPRT(template, args) => CallST(translateTemplex(userDeclaredRunes, template), args.map(translateTemplex(userDeclaredRunes, _)))
      case FunctionPRT(mutability, paramsPack, returnType) => {
        CallST(
          NameST("IFunction"),
          List(
            mutability match { case None => MutabilityST(MutableP) case Some(m) => translateTemplex(userDeclaredRunes, m) },
            translateTemplex(userDeclaredRunes, paramsPack),
            translateTemplex(userDeclaredRunes, returnType)))
      }
      case PrototypePRT(name, parameters, returnType) => PrototypeST(name, parameters.map(translateTemplex(userDeclaredRunes, _)), translateTemplex(userDeclaredRunes, returnType))
      case PackPRT(members) => PackST(members.map(translateTemplex(userDeclaredRunes, _)))
      case RepeaterSequencePRT(mutability, size, element) => RepeaterSequenceST(translateTemplex(userDeclaredRunes, mutability), translateTemplex(userDeclaredRunes, size), translateTemplex(userDeclaredRunes, element))
      case ManualSequencePRT(elements) => ManualSequenceST(elements.map(translateTemplex(userDeclaredRunes, _)))
    }
  }
}
