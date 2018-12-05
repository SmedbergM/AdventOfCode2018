package day5

import org.scalatest.FlatSpec

class Day5Spec extends FlatSpec {
  "simplify" should "cancel inverse pairs" in {
    val input0 = ""
    assertResult("")(Day5.simplify(input0))

    val input1 = "aA"
    assertResult("")(Day5.simplify(input1))

    val input2 = "cBbB"
    assertResult("cB")(Day5.simplify(input2))

    val input3 = "dabAcCaCBAcCcaDA"
    assertResult("dabCBAcaDA")(Day5.simplify(input3))
  }

  "task2" should "remove the monomer which leaves the simplest polymer" in {
    val input = "dabAcCaCBAcCcaDA"
    assertResult(("c/C", 4))(Day5.task2(input))
  }
}
