package net.verdagon.radonc

object Pattern {
  val code =
    """
      |struct Location imm {
      |  groupX: Int;
      |  groupY: Int;
      |  indexInGroup: Int;
      |}
      |
      |struct Pattern imm {
      |  name: Str;
      |  cornersByShapeIndex: __Array:(imm, __Array<imm, Vec2>);
      |  patternTiles: __Array<imm, PatternTile>;
      |  xOffset: Vec2;
      |  yOffset: Vec2;
      |}
      |
      |struct PatternTile imm {
      |  shapeIndex: Int;
      |  rotateRadians: Float;
      |  translate: Vec2;
      |  sideAdjacenciesBySideIndex: __Array<imm, PatternSideAdjacency>;
      |  cornerAdjacenciesByCornerIndex: __Array:(imm, __Array<imm, PatternCornerAdjacency>);
      |}
      |
      |struct PatternSideAdjacency imm {
      |  groupRelativeX: Int;
      |  groupRelativeY: Int;
      |  tileIndex: Int;
      |  sideIndex: Int;
      |}
      |
      |struct PatternCornerAdjacency imm {
      |  groupRelativeX: Int;
      |  groupRelativeY: Int;
      |  tileIndex: Int;
      |  cornerIndex: Int;
      |}
      |
    """.stripMargin
}
