package day22

import org.scalatest.FlatSpec

class Day22Spec extends FlatSpec {
  "Cave" should "find a shortest path" in {
    val cave = new Cave(510, XY(10,10))
    assertResult(45)(cave.fastestPathToTarget())
  }
}
