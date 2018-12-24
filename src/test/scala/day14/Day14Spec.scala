package day14

import day14.Day14.RecipeBoard
import org.scalatest.FlatSpec

class Day14Spec extends FlatSpec {
  "RecipeBoard.next" should "step forward" in {
    val board0 = RecipeBoard.initial
    val board1 = board0.next
    assertResult(RecipeBoard("3710", 0, 1))(board1)

    val board2 = board1.next
    assertResult(RecipeBoard("371010", 4, 3))(board2)

    val board3 = board2.next
    assertResult(RecipeBoard("3710101",6,4))(board3)

    val board4 = board3.next
    assertResult(RecipeBoard("37101012", 0, 6))(board4)
  }

  "Day14.task1" should "drop N recipes" in {
    assertResult("0124515891")(Day14.task1(5))
    assertResult("5158916779")(Day14.task1(9))
    assertResult("9251071085")(Day14.task1(18))
    assertResult("5941429882")(Day14.task1(2018))
  }

  "Day14.task2" should "find the first occurrence of a given string" in {
    assertResult(5)(Day14.task2("01245"))
    assertResult(9)(Day14.task2("51589"))
    assertResult(18)(Day14.task2("92510"))
    assertResult(2018)(Day14.task2("59414"))
  }
}
