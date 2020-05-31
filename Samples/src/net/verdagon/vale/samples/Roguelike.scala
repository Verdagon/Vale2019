package net.verdagon.vale.samples

import net.verdagon.vale.{HashMap, Opt, OptingArrayList}

object Roguelike {
  val code: String =
    Opt.code +
      OptingArrayList.code +
      HashMap.code +
      """
        |struct Vec2 imm {
        |  x Float;
        |  y Float;
        |}
        |
        |struct Vec3 imm {
        |  x Float;
        |  y Float;
        |  z Float;
        |}
        |
        |struct Location imm {
        |  groupX Int;
        |  groupY Int;
        |  indexInGroup Int;
        |}
        |
        |struct Pattern imm {
        |  name Str;
        |  cornersByShapeIndex Array<imm, Array<imm, Vec2>>;
        |  patternTiles Array<imm, PatternTile>;
        |  xOffset Vec2;
        |  yOffset Vec2;
        |}
        |
        |struct PatternTile imm {
        |  shapeIndex Int;
        |  rotateRadians Float;
        |  translate Vec2;
        |  sideAdjacenciesBySideIndex Array<imm, PatternSideAdjacency>;
        |  cornerAdjacenciesByCornerIndex Array<imm, Array<imm, PatternCornerAdjacency>>;
        |}
        |
        |struct PatternSideAdjacency imm {
        |  groupRelativeX Int;
        |  groupRelativeY Int;
        |  tileIndex Int;
        |  sideIndex Int;
        |}
        |
        |struct PatternCornerAdjacency imm {
        |  groupRelativeX Int;
        |  groupRelativeY Int;
        |  tileIndex Int;
        |  cornerIndex Int;
        |}
        |
        |
        |struct TerrainTile {
        |  elevation Int;
        |  walkable Bool;
        |  classId Str;
        |}
        |
        |struct LocationHasher { }
        |fn __call(this &LocationHasher, loc Location) {
        |  hash! = 0;
        |  mut hash = 41 * hash + loc.groupX;
        |  mut hash = 41 * hash + loc.groupY;
        |  mut hash = 41 * hash + loc.indexInGroup;
        |  = hash;
        |}
        |
        |struct LocationEquator { }
        |fn __call(this &LocationEquator, a Location, b Location) {
        |  (a.groupX == b.groupX) and (a.groupY == b.groupY) and (a.indexInGroup == b.indexInGroup)
        |}
        |
        |struct Terrain {
        |  pattern Pattern;
        |  elevationStepHeight Float;
        |  tiles HashMap<Location, TerrainTile, LocationHasher, LocationEquator>;
        |}
        |
        |fn for<F>(beginInclusive Int, endExclusive Int, func F) {
        |  i! = beginInclusive;
        |  while (i < endExclusive) {
        |    func(i);
        |    mut i = i + 1;
        |  }
        |}
        |
        |fn main() {
        |  board =
        |      Array<mut, Array<mut, Str>>(10, &IFunction1<mut, Int, Array<mut, Str>>((row){
        |        Array<mut, Str>(10, &IFunction1<mut, Int, Str>((col){
        |          = if (row == 0) { "#" }
        |            else if (col == 0) { "#" }
        |            else if (row == 9) { "#" }
        |            else if (col == 9) { "#" }
        |            else { "." }
        |        }))
        |      }));
        |
        |  playerRow! = 4;
        |  playerCol! = 3;
        |
        |  running! = true;
        |  while (running) {
        |    for(0, 10, (rowI){
        |      row = board.(rowI);
        |      for(0, 10, (colI){
        |        cell = row.(colI);
        |        if (and(rowI == playerRow, colI == playerCol)) {
        |          print("@");
        |        } else {
        |          print(cell);
        |        }
        |      });
        |      println("");
        |    });
        |
        |    key = __getch();
        |    println(key);
        |    if (key == 81) {
        |      mut running = false;
        |    } else if (key == 119) {
        |      mut playerRow = playerRow - 1;
        |    } else if (key == 115) {
        |      mut playerRow = playerRow + 1;
        |    } else if (key == 97) {
        |      mut playerCol = playerCol - 1;
        |    } else if (key == 100) {
        |      mut playerCol = playerCol + 1;
        |    }
        |  }
        |}
        |""".stripMargin
}