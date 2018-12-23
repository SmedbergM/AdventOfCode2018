package day12

import org.scalatest.FlatSpec
import Day12._

class Day12Spec extends FlatSpec {
  "parseChallenge" should "parse a challenge" in {
    val challenge =
      """initial state: #..#.#..##......###...###
        |
        |...## => #
        |..#.. => #
        |.#... => #
        |.#.#. => #
        |.#.## => #
        |.##.. => #
        |.#### => #
        |#.#.# => #
        |#.### => #
        |##.#. => #
        |##.## => #
        |###.. => #
        |###.# => #
        |####. => #
        |
      """.stripMargin

    val (optState, rewriteRules) = Day12.parseChallenge(challenge)
    val state0 = optState match {
      case Some(State(flowerPots, j)) if j == 0 =>
        assertResult("#..#.#..##......###...###")(flowerPots)
        State(flowerPots, 0)
      case _ => fail("Parsing failed")
    }

    assertResult(14)(rewriteRules.rules.size)
    rewriteRules.rules.foreach { case (pattern, result) =>
      assertResult('#')(result)
    }

    val state1 = state0.evolve(rewriteRules)
    assertResult(State("#...#....#.....#..#..#..#", 0))(state1)
  }

  "evolve" should "evolve according to rewrite rules" in {
    val challenge =
      """initial state: #..#.#..##......###...###
        |
        |...## => #
        |..#.. => #
        |.#... => #
        |.#.#. => #
        |.#.## => #
        |.##.. => #
        |.#### => #
        |#.#.# => #
        |#.### => #
        |##.#. => #
        |##.## => #
        |###.. => #
        |###.# => #
        |####. => #
        |
      """.stripMargin

    val (state0, rewriteRules) = {
      val (optState0, rewriteRules) = Day12.parseChallenge(challenge)
      optState0.get -> rewriteRules
    }

    val state1 = state0.evolve(rewriteRules)
    assertResult(State("#...#....#.....#..#..#..#", 0))(state1)

    val state2 = state1.evolve(rewriteRules)
    assertResult(State("##..##...##....#..#..#..##", 0))(state2)

    val state3 = state2.evolve(rewriteRules)
    assertResult(State("#.#...#..#.#....#..#..#...#", -1))(state3)
  }
}
