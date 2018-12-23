package day9

import org.scalatest.FlatSpec

class Day9Spec extends FlatSpec {
  "MarbleGame" should "play the marble game" in {
    val m0 = MarbleGame.empty(9)

    val m1 = m0.play match {
      case mg@MarbleGame(_, score, Vector(1,0), 2) =>
        score.foreach { case (p, s) =>
          assertResult(0)(s)
        }
        mg
    }

    val m2 = m1.play match {
      case mg@MarbleGame(_, score, Vector(2,1,0), 3) =>
        score.foreach{ case (p,s) =>
          assertResult(p -> 0)(p -> s)
        }
        mg
    }

    val m3 = m2.play match {
      case mg@MarbleGame(_, score, Vector(3,0,2,1), 4) =>
        score.foreach{ case (p,s) =>
          assertResult(p -> 0)(p -> s)
        }
        mg
    }

    val m4 = m3.play match {
      case mg@MarbleGame(_, _, Vector(4,2,1,3,0), 5) => mg
    }

    val m5 = m4.play match {
      case mg@MarbleGame(_, _, Vector(5,1,3,0,4,2), 6) => mg
    }

    val m22 = List.range(6, 23).foldLeft(m5) { case (mg, k) =>
      assertResult(k)(mg.nextMarble)
      mg.play
    }

    val m23 = m22.play match {
      case mg@MarbleGame(_, score, Vector(19, 2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15, 0, 16, 8, 17, 4, 18), 24) =>
        score.foreach {
          case (5, 32) => // OK
          case (p, s) => assertResult(p -> 0)(p -> s)
        }
        mg
    }

    val m24 = m23.play match {
      case mg@MarbleGame(_, _, marbles, 25) =>
        assertResult(24)(marbles.head)
        assertResult(2)(marbles.last)
    }
  }
}
