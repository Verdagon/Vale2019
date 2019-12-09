package net.verdagon.radonc.templar.infer

import net.verdagon.radonc.templar.templata.ITemplata
import net.verdagon.radonc.{vassert, vwat}


case class Inferences(
  templatasByRune: Map[String, ITemplata],
  possibilitiesByRune: Map[String, List[ITemplata]]) {
  def addConclusion(rune: String, templata: ITemplata): Inferences = {
    templatasByRune.get(rune) match {
      case None =>
      case Some(existingConclusion) => vassert(templata == existingConclusion)
    }
    Inferences(
      templatasByRune + (rune -> templata),
      possibilitiesByRune - rune)
  }
  def addPossibilities(rune: String, possibilities: List[ITemplata]): Inferences = {
    if (possibilities.size == 0) {
      vwat()
    } else if (possibilities.size == 1) {
      addConclusion(rune, possibilities.head)
    } else {
      vassert(!templatasByRune.contains(rune))
      possibilitiesByRune.get(rune) match {
        case None =>
        case Some(existingPossibilities) => vassert(possibilities == existingPossibilities)
      }
      Inferences(
        templatasByRune,
        possibilitiesByRune + (rune -> possibilities))
    }
  }
  // Returns an Inferences without this rune, and gives all the possibilities for that rune
  def pop(rune: String): (Inferences, List[ITemplata]) = {
    val inferencesWithoutThatRune = Inferences(templatasByRune, possibilitiesByRune - rune)
    (inferencesWithoutThatRune, possibilitiesByRune(rune))
  }
}

case class InferencesBox(var inferences: Inferences) {
  def templatasByRune: Map[String, ITemplata] = inferences.templatasByRune
  def possibilitiesByRune: Map[String, List[ITemplata]] = inferences.possibilitiesByRune

  def addConclusion(rune: String, templata: ITemplata): Unit = {
    inferences = inferences.addConclusion(rune, templata)
  }
  def addPossibilities(rune: String, possibilities: List[ITemplata]): Unit = {
    inferences = inferences.addPossibilities(rune, possibilities)
  }
  // Returns an Inferences without this rune, and gives all the possibilities for that rune
  def pop(rune: String): List[ITemplata] = {
    val (newInferences, result) = inferences.pop(rune)
    inferences = newInferences
    result
  }
}

