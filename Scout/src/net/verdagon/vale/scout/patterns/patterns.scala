package net.verdagon.vale.scout.patterns

import net.verdagon.vale.parser.{CaptureP, VariabilityP}

import scala.collection.immutable.List

case class AtomSP(
  // This is an option because in PatternTemplar, if it's None, we'll explode the
  // expression into the destructure, and if it's Some, we'll make this variable
  // an owning ref.
  name: Option[CaptureP],
  virtuality: Option[VirtualitySP],
  coordRune: String,
  destructure: Option[List[Option[AtomSP]]])

sealed trait VirtualitySP
case object AbstractSP extends VirtualitySP
case class OverrideSP(kindRune: String) extends VirtualitySP

object PatternSUtils {
  def getDistinctOrderedRunesForPattern(pattern: AtomSP): List[String] = {
    val runesFromVirtuality =
      pattern.virtuality match {
        case None => List()
        case Some(AbstractSP) => List()
        case Some(OverrideSP(kindRune)) => List(kindRune)
      }
    val runesFromDestructures =
      pattern.destructure.toList.flatten.flatten.flatMap(getDistinctOrderedRunesForPattern)
    (runesFromVirtuality ++ runesFromDestructures :+ pattern.coordRune).distinct
  }

}