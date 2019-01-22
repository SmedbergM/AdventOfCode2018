package day23

import org.scalatest.FlatSpec

class Day23Spec extends FlatSpec {
  "Nanobot" should "parse a nanobot" in {
    val input0 = "pos=<2,0,0>, r=4"
    assertResult(Some(Nanobot(2,0,0,4)))(Nanobot.parse(input0))

    val input1 = s"pos=<-10,${Int.MaxValue}0,3>, r=123"
    assertResult(Some(Nanobot(-10, Int.MaxValue.toLong * 10, 3, 123)))(Nanobot.parse(input1))

    val input2 = "pos=<0,0,0>, r=-4"
    assertResult(None)(Nanobot.parse(input2))
  }
}
