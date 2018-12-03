package day2

import org.scalatest.FlatSpec

class Day2Spec extends FlatSpec {
  "task1" should "checksum a list of strings" in {
    val input0 = Nil
    assertResult(0)(Day2.task1(input0))

    val input1 = List(
      "abcdef",
      "bababc",
      "abbcde",
      "abcccd",
      "aabcdd",
      "abcdee",
      "ababab"
    )
    assertResult(12)(Day2.task1(input1))
  }

  "task2" should "find the common characters of the two strings differing by one character" in {
    val input0 = List(
      "abc", "acc"
    )
    assertResult(Some("ac"))(Day2.task2(input0))

    val input1 = List(
      "abcde",
      "fghij",
      "klmno",
      "pqrst",
      "fguij",
      "axcye",
      "wvxyz"
    )
    assertResult(Some("fgij"))(Day2.task2(input1))
  }
}
