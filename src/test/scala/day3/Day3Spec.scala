package day3

import org.scalatest.FlatSpec

class Day3Spec extends FlatSpec {
  "parseClaim" should "parse a claim" in {
    val input0 = "#1 @ 1,3: 4x4"
    assertResult(Some(Claim(1, 1, 3, 4, 4)))(Day3.parseClaim(input0))
  }

  val input = Claim(1, 1, 3, 4, 4) ::
    Claim(2, 3, 1, 4, 4) ::
    Claim (3, 5, 5, 2, 2) :: Nil

  "task1" should "count disputed squares" in {
    assertResult(4)(Day3.task1(input))
  }

  "task2" should "find an undisputed claim" in {
    assertResult(Some(3))(Day3.task2(input))
  }
}
