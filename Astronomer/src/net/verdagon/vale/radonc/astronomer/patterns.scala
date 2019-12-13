package net.verdagon.vale.astronomer

import net.verdagon.vale.parser.{CaptureP, VariabilityP}

import scala.collection.immutable.List

case class AtomAP(
  // This is an option because in PatternTemplar, if it's None, we'll explode the
  // expression into the destructure, and if it's Some, we'll make this variable
  // an owning ref.
  name: Option[CaptureP],
  virtuality: Option[VirtualityAP],
  coordRune: String,
  destructure: Option[List[Option[AtomAP]]])

sealed trait VirtualityAP
case object AbstractAP extends VirtualityAP
case class OverrideAP(kindRune: String) extends VirtualityAP

object PatternSUtils {
  def getDistinctOrderedRunesForPattern(pattern: AtomAP): List[String] = {
    val runesFromVirtuality =
      pattern.virtuality match {
        case None => List()
        case Some(AbstractAP) => List()
        case Some(OverrideAP(kindRune)) => List(kindRune)
      }
    val runesFromDestructures =
      pattern.destructure.toList.flatten.flatten.flatMap(getDistinctOrderedRunesForPattern)
    (runesFromVirtuality ++ runesFromDestructures :+ pattern.coordRune).distinct
  }

}