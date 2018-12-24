package day13

import day13.Day13.CartSystem
import org.scalatest.FlatSpec

class Day13Spec extends FlatSpec {
  "CartSystem.parse" should "parse a Cart System" in {
    import Day13.XY

    val input =
      """/->-\
        ||   |  /----\
        || /-+--+-\  |
        || | |  | v  |
        |\-+-/  \-+--/
        |  \------/
      """.stripMargin

    val cartSystem = CartSystem.parse(input)
    assert(cartSystem.tracks.isValid)
    assertResult(Set(XY(2,0), XY(9, 3)))(cartSystem.carts.keySet)
    assertResult(XY(2,0) :: XY(9,3) :: Nil)(cartSystem.waitingToMove)
  }

  "cartSystem.step" should "step" in {
    import Day13.XY
    val input =
      """/->-\
        ||   |  /----\
        || /-+--+-\  |
        || | |  | v  |
        |\-+-/  \-+--/
        |  \------/
      """.stripMargin

    val cartSystem = CartSystem.parse(input)
    val cartSystem1 = cartSystem.step
    assertResult(XY(9,3) :: Nil)(cartSystem1.waitingToMove)
    assert(cartSystem1.carts.contains(XY(3,0)))
    assert(!cartSystem1.carts.contains(XY(2,0)))

    val cartSystem2 = cartSystem1.step
    assertResult(Set(XY(9,4), XY(3,0)))(cartSystem2.carts.keySet)
    assertResult(XY(3,0) :: XY(9,4) :: Nil)(cartSystem2.waitingToMove)
    assertResult(Day13.East)(cartSystem2.carts(XY(9,4)).heading)
    assertResult(Day13.Straight)(cartSystem2.carts(XY(9,4)).nextTurn)

    val cartSystem3 = cartSystem2.step.step
    assertResult(Day13.South)(cartSystem3.carts(XY(4,0)).heading)
    assertResult(Day13.East)(cartSystem3.carts(XY(10,4)).heading)

    val cartSystem4 = cartSystem3.step.step.step.step
    assertResult(Day13.East)(cartSystem4.carts(XY(4,2)).heading)
    assertResult(Day13.North)(cartSystem4.carts(XY(12, 4)).heading)

    val cartSystem5 = cartSystem4.step.step.step.step.step.step
    assertResult(Day13.East)(cartSystem5.carts(XY(7,2)).heading)
    assertResult(Day13.Right)(cartSystem5.carts(XY(7,2)).nextTurn)
    assertResult(Day13.West)(cartSystem5.carts(XY(12,1)).heading)
    assertResult(XY(12,1) :: XY(7,2) :: Nil)(cartSystem5.waitingToMove)

    val cartSystem6 = (0 until 6).foldLeft(cartSystem5)((cs,_) => cs.step.step)
    assertResult(Day13.South)(cartSystem6.carts(XY(7,2)).heading)
    assertResult(Day13.North)(cartSystem6.carts(XY(7,4)).heading)

    assertThrows[Day13.Collision](cartSystem6.step.step)
  }

  "CartSystem.stepAndClearWreckage" should "clear collisions" in {
    import Day13.XY
    val input = """/>-<\
                  ||   |
                  || /<+-\
                  || | | v
                  |\>+</ |
                  |  |   ^
                  |  \<->/""".stripMargin

    val cartSystem0 = CartSystem.parse(input)
    assertThrows[Day13.Collision](cartSystem0.step.step)

    val cartSystem1 = cartSystem0.step.stepAndClearWreckage
    assertResult(7)(cartSystem1.carts.size)

    val cartSystem2 = cartSystem1.step.step.step.stepAndClearWreckage.stepAndClearWreckage.step.step
    assertResult(Set(XY(2,2), XY(2, 6), XY(6,6)))(cartSystem2.carts.keySet)

    val cartSystem3 = cartSystem2.step.step.step.step.stepAndClearWreckage
    assertResult(Set(XY(6,5)))(cartSystem3.carts.keySet)

    assertResult(XY(6,4))(Day13.lastCartStanding(cartSystem0))
  }
}
