package day17

import org.scalatest.FlatSpec

class Day17Spec extends FlatSpec {
  "GeologicalSurvey.parse" should "parse a response body" in {
    val input =
      """x=495, y=2..7
        |y=7, x=495..501
        |x=501, y=3..7
        |x=498, y=2..4
        |x=506, y=1..2
        |x=498, y=10..13
        |x=504, y=10..13
        |y=13, x=498..504
      """.stripMargin

    val geologicalSurvey = GeologicalSurvey.parse(input)
    assertResult(34)(geologicalSurvey.clays.size)
    assertResult(495)(geologicalSurvey.clays.map(_.x).min)
    assertResult(506)(geologicalSurvey.clays.map(_.x).max)
    assertResult(1)(geologicalSurvey.clays.map(_.y).min)
    assertResult(13)(geologicalSurvey.clays.map(_.y).max)
  }

  "geologicalSurvey.hydrate" should "let the waters flow" in {
    val input =
      """x=495, y=2..7
        |y=7, x=495..501
        |x=501, y=3..7
        |x=498, y=2..4
        |x=506, y=1..2
        |x=498, y=10..13
        |x=504, y=10..13
        |y=13, x=498..504
      """.stripMargin

    val geologicalSurvey = GeologicalSurvey.parse(input)

    val h = geologicalSurvey.hydrate()
    assertResult(57)(h.size)
  }
}
