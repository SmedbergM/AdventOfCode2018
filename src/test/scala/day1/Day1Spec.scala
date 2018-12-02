package day1

import org.scalatest.FlatSpec

class Day1Spec extends FlatSpec {
  "parseChallenge" should "convert a challenge string into a sequence" in {
    val input0 = ""
    assert(Day1.parseChallenge(input0).isEmpty)

    val input1 = "\n\n"
    assert(Day1.parseChallenge(input1).isEmpty)

    val input2 = "\nt\n"
    assert(Day1.parseChallenge(input2).isEmpty)

    val input3 = "3"
    assert(Day1.parseChallenge(input3).isEmpty)

    val input4 = "+3"
    assert(Day1.parseChallenge(input4).toList == List(3))

    val input5 = "+3\n"
    assert(Day1.parseChallenge(input5).toList == List(3))

    val input6 = "-1\n+1\n"
    assert(Day1.parseChallenge(input6).toList == List(-1,1))
  }

  "task2" should "find the first repeated running total" in {
    val input0 = List(-1,1)
    assertResult(0)(Day1.task2(input0))

    val input1 = List(+3, +3, +4, -2, -4)
    assertResult(10)(Day1.task2(input1))

    val input2 = List(-6, +3, +8, +5, -6)
    assertResult(5)(Day1.task2(input2))

    val input3 = List(+7, +7, -2, -7, -4)
    assertResult(14)(Day1.task2(input3))
  }
}
