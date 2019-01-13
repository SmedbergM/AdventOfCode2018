package day20

import ForkedPath._
import Cardinal._
import org.scalatest.FlatSpec

class Day20Spec extends FlatSpec {
  "ForkedPath" should "parse a string" in {
    val input = """^ENWWW(NEEE|SSE(EE|N))W$"""
    val forkedPath = ForkedPath.parse(input)
    val tail1 = forkedPath match {
      case OneWay(East, tail) => tail
      case _ => fail("Bad parse")
    }
    val tail5 = tail1 match {
      case OneWay(North, OneWay(West, OneWay(West, OneWay(West, tail)))) => tail
      case _ => fail("Bad parse")
    }
    tail5 match {
      case Fork(f0 :: f1 :: Nil, OneWay(West, Empty)) =>
        assertResult(OneWay(North, OneWay(East, OneWay(East, OneWay(East, Empty)))))(f0)
        val fork2 = Fork(List(OneWay(East, OneWay(East, Empty)), OneWay(North, Empty)), Empty)
        assertResult(OneWay(South, OneWay(South, OneWay(East, fork2))))(f1)
      case _ => fail("Bad parse")
    }
  }
}
