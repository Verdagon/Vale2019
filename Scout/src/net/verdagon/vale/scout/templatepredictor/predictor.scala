package net.verdagon.vale.scout

import net.verdagon.vale.scout.rules.ITypeSR

package object predictor {
  case class ConclusionsBox(var conclusions: Conclusions) {
    def knowableValueRunes: Set[String] = conclusions.knowableValueRunes
    def predictedTypeByRune: Map[String, ITypeSR] = conclusions.predictedTypeByRune
    def markRuneValueKnowable(rune: String): Unit = {
      conclusions = conclusions.markRuneValueKnowable(rune)
    }
    def markRuneTypeKnown(rune: String, tyype: ITypeSR): Unit = {
      conclusions = conclusions.markRuneTypeKnown(rune, tyype)
    }
  }

  case class Conclusions(
      knowableValueRunes: Set[String],
      predictedTypeByRune: Map[String, ITypeSR]) {
    def markRuneValueKnowable(rune: String): Conclusions = {
      Conclusions(
        knowableValueRunes + rune,
        predictedTypeByRune)
    }
    def markRuneTypeKnown(rune: String, tyype: ITypeSR): Conclusions = {
      Conclusions(
        knowableValueRunes,
        predictedTypeByRune + (rune -> tyype))
    }
  }
}
