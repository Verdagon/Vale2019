package net.verdagon.vale.scout

import net.verdagon.vale.scout.rules.ITypeSR

package object predictor {
  case class ConclusionsBox(var conclusions: Conclusions) {
    def knowableValueRunes: Set[AbsoluteNameS[IRuneS]] = conclusions.knowableValueRunes
    def predictedTypeByRune: Map[AbsoluteNameS[IRuneS], ITypeSR] = conclusions.predictedTypeByRune
    def markRuneValueKnowable(rune: AbsoluteNameS[IRuneS]): Unit = {
      conclusions = conclusions.markRuneValueKnowable(rune)
    }
    def markRuneTypeKnown(rune: AbsoluteNameS[IRuneS], tyype: ITypeSR): Unit = {
      conclusions = conclusions.markRuneTypeKnown(rune, tyype)
    }
  }

  case class Conclusions(
      knowableValueRunes: Set[AbsoluteNameS[IRuneS]],
      predictedTypeByRune: Map[AbsoluteNameS[IRuneS], ITypeSR]) {
    def markRuneValueKnowable(rune: AbsoluteNameS[IRuneS]): Conclusions = {
      Conclusions(
        knowableValueRunes + rune,
        predictedTypeByRune)
    }
    def markRuneTypeKnown(rune: AbsoluteNameS[IRuneS], tyype: ITypeSR): Conclusions = {
      Conclusions(
        knowableValueRunes,
        predictedTypeByRune + (rune -> tyype))
    }
  }
}
