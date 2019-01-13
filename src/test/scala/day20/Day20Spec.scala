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

  "FloorPlan" should "build a FloorPlan from a forked path" in {
    val input = "^ENWWW(NEEE|SSE(EE|N))$"
    val basePath = ForkedPath.parse(input)
    val floorPlan = FloorPlan.build(basePath)
    val (nRooms, nNS, nEW) = floorPlan.squares.foldLeft((0,0,0)){
      case ((r, ns, ew), (_, GridSquare.Room)) => (r+1, ns, ew)
      case ((r, ns, ew), (_, GridSquare.NSDoor)) => (r, ns + 1, ew)
      case ((r, ns, ew), (_, GridSquare.EWDoor)) => (r, ns, ew + 1)
    }
    assertResult(16)(nRooms)
    assertResult(5)(nNS)
    assertResult(10)(nEW)

    val input2 = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
    val basePath2 = ForkedPath.parse(input2)
    val floorPlan2 = FloorPlan.build(basePath2)
    val (nRooms2, nNS2, nEW2) = floorPlan2.squares.foldLeft((0,0,0)){
      case ((r, ns, ew), (_, GridSquare.Room)) => (r+1, ns, ew)
      case ((r, ns, ew), (_, GridSquare.NSDoor)) => (r, ns + 1, ew)
      case ((r, ns, ew), (_, GridSquare.EWDoor)) => (r, ns, ew + 1)
    }
    assertResult(25)(nRooms2)
    assertResult(13)(nNS2)
    assertResult(11)(nEW2)
  }

  it should "find the farthest room" in {
    val input0 = "^WNE$"
    val floorPlan0 = FloorPlan.build(ForkedPath.parse(input0))
    val bfs0 = floorPlan0.farthestRoom
    assertResult(XY(0, -2) -> 3)(bfs0)

    val input1 = "^ENWWW(NEEE|SSE(EE|N))$"
    val floorPlan1 = FloorPlan.build(ForkedPath.parse(input1))
    val bfs1 = floorPlan1.farthestRoom
    assertResult(XY(2,2) -> 10)(bfs1)

    val input2 = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
    val floorPlan2 = FloorPlan.build(ForkedPath.parse(input2))
    val bfs2 = floorPlan2.farthestRoom
    assertResult(XY(4,-4) -> 18)(bfs2)

    val input3 = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
    val floorPlan3 = FloorPlan.build(ForkedPath.parse(input3))
    val bfs3 = floorPlan3.farthestRoom
    assertResult(23)(bfs3._2)

    val input4 = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
    val floorPlan4 = FloorPlan.build(ForkedPath.parse(input4))
    val bfs4 = floorPlan4.farthestRoom
    assertResult(31)(bfs4._2)
  }
}
