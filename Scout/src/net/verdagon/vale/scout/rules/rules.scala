package net.verdagon.vale.scout.rules

import net.verdagon.vale.scout._

import scala.collection.immutable.List

sealed trait IRulexSR
case class EqualsSR(left: IRulexSR, right: IRulexSR) extends IRulexSR
case class IsaSR(sub: IRulexSR, suuper: IRulexSR) extends IRulexSR
case class OrSR(alternatives: List[IRulexSR]) extends IRulexSR
case class ComponentsSR(
  // This is a TypedSR so that we can know the type, so we can know whether this is
  // a kind components rule or a coord components rule.
  container: TypedSR,
  components: List[IRulexSR]
) extends IRulexSR
case class TypedSR(rune: AbsoluteNameS[IRuneS], tyype: ITypeSR) extends IRulexSR
case class TemplexSR(templex: ITemplexS) extends IRulexSR
// This is for built-in parser functions, such as exists() or isBaseOf() etc.
case class CallSR(name: String, args: List[IRulexSR]) extends IRulexSR {
}

sealed trait ITypeSR
case object IntTypeSR extends ITypeSR
case object BoolTypeSR extends ITypeSR
case object OwnershipTypeSR extends ITypeSR
case object MutabilityTypeSR extends ITypeSR
case object PermissionTypeSR extends ITypeSR
case object LocationTypeSR extends ITypeSR
case object CoordTypeSR extends ITypeSR
case object KindTypeSR extends ITypeSR
case object FunctionTypeSR extends ITypeSR
case class TemplateTypeSR(params: List[ITypeSR], result: ITypeSR) extends ITypeSR
case object VariabilityTypeSR extends ITypeSR
//case object StructTypeSR extends ITypeSR
//case object SequenceTypeSR extends ITypeSR
// We need PackTypeSR because we have a built-in templated destructor whose rules
// only match packs... PackTypeSR is how it does that.
//case object PackTypeSR extends ITypeSR
//case object ArrayTypeSR extends ITypeSR
//case object CallableTypeSR extends ITypeSR
//case object InterfaceTypeSR extends ITypeSR

object RuleSUtils {

  def getDistinctOrderedRunesForRulex(envFullName: AbsoluteNameS[INameS], rulex: IRulexSR): List[AbsoluteNameS[IRuneS]] = {
    rulex match {
      case EqualsSR(left, right) => (getDistinctOrderedRunesForRulex(envFullName, left) ++ getDistinctOrderedRunesForRulex(envFullName, right)).distinct
      case IsaSR(left, right) => (getDistinctOrderedRunesForRulex(envFullName, left) ++ getDistinctOrderedRunesForRulex(envFullName, right)).distinct
      case OrSR(possibilities) => possibilities.flatMap(getDistinctOrderedRunesForRulex(envFullName, _)).distinct
      case ComponentsSR(container, components) => {
        getDistinctOrderedRunesForRulex(envFullName, container) ++
          components.flatMap(getDistinctOrderedRunesForRulex(envFullName, _)).toSet
      }
      case TypedSR(rune, tyype) => List(rune)
      case TemplexSR(templex) => TemplexSUtils.getDistinctOrderedRunesForTemplex(templex)
      case CallSR(name, args) => args.flatMap(getDistinctOrderedRunesForRulex(envFullName, _)).distinct
    }
  }

  def getDistinctOrderedRunesForRulexes(envFullName: AbsoluteNameS[INameS], rulexes: List[IRulexSR]):
  List[AbsoluteNameS[IRuneS]] = {
    rulexes.flatMap(getDistinctOrderedRunesForRulex(envFullName, _)).distinct
  }

  // This can make a ref for the given kind, choosing the appropriate ownership.
  // However, it can't figure out an unknown kind given a coord, so it's not that
  // useful in inferring.
  // We COULD make it possible to infer through this. Might be worth considering.
  def knownCoordRule(kindRulexSR: IRulexSR): IRulexSR = {
    CallSR("toRef", List(kindRulexSR))
  }
}
