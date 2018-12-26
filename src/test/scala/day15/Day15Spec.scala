package day15

import org.scalatest.FlatSpec

class Day15Spec extends FlatSpec {
  "BattleBoard.parse" should "parse a BattleBoard" in {
    val input =
      """#######
        |#.G.E.#
        |#E.G.E#
        |#.G.E.#
        |#######
      """.stripMargin

    val battleBoard = BattleBoard.parse(input)
    assert(battleBoard.isValid)
    assert(battleBoard.isOngoing)
    assertResult((4,3))(battleBoard.battleUnits.foldLeft((0,0)){
      case ((e, g), (_, Elf(_, _))) => (e+1, g)
      case ((e,g), (_, Goblin(_))) => (e, g + 1)
    })
  }

  "battleBoard" should "move and do battle" in {
    val input =
      """#######
        |#E..G.#
        |#...#.#
        |#.G.#G#
        |#######
      """.stripMargin
    val battleBoard = BattleBoard.parse(input)

    val moveResult = battleBoard.move()
    assertResult(Moved(XY(2,1), Elf(200)))(moveResult)
    assertResult(NoBattle)(battleBoard.doBattle(moveResult))

    val input2 =
      """#######
        |#..EG.#
        |#...#.#
        |#.G.#G#
        |#######
      """.stripMargin
    val battleBoard2 = BattleBoard.parse(input2)
    val moveResult2 = battleBoard2.move()
    assertResult(AdjacentToEnemy(XY(3,1),Elf(200), nonEmptyList(XY(4,1))))(moveResult2)
    assertResult(Damage(XY(4,1), Goblin(197)))(battleBoard2.doBattle(moveResult2))
    val battleBoard2a = battleBoard2.takeTurn
    assertResult(Goblin(197))(battleBoard2a.battleUnits(XY(4,1)))


    val input3 = """#######
                   |#.G.E.#
                   |#G.G..#
                   |#.GGGG#
                   |#######
                 """.stripMargin
    val battleBoard3 = {
      val board = BattleBoard.parse(input3)
      val nextAwaitingTurn = nonEmptyList(XY(4,1))
      val nextBattleUnits = board.battleUnits + (XY(2,1) -> Goblin(3))
      board.copy(awaitingTurn = nextAwaitingTurn, battleUnits = nextBattleUnits)
    }

    val moveResult3 = battleBoard3.move()
    moveResult3 match {
      case AdjacentToEnemy(XY(3,1), Elf(200, _), enemies) =>
        assertResult(Set(XY(2,1), XY(3,2)))(enemies.toSet)
      case _ => fail()
    }
    assertResult(Kill(XY(2,1)))(battleBoard3.doBattle(moveResult3))

    val battleBoard4 = battleBoard3.copy(awaitingTurn = nonEmptyList(XY(3,3)))
    assertResult(NoPossibleMoves)(battleBoard4.move())

    val battleBoard5 = battleBoard4.copy(awaitingTurn = nonEmptyList(XY(2,3)))
    assertResult(NoPossibleMoves)(battleBoard5.move())
  }
}
