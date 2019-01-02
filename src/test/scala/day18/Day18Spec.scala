package day18

import org.scalatest.FlatSpec

class Day18Spec extends FlatSpec {
  "Chart" should "parse input" in {
    val input =
      """.#.#...|#.
        |.....#|##|
        |.|..|...#.
        |..|#.....#
        |#.#|||#|#|
        |...#.||...
        |.|....|...
        |||...#|.#|
        ||.||||..|.
        |...#.|..|.""".stripMargin

    val chart = Chart.parse(input)
    assertResult((56, 27, 17)){ chart.acres.foldLeft((0,0,0)){
      case ((open, trees, lumber), (_, Open)) => (open + 1, trees, lumber)
      case ((open, trees, lumber), (_, Trees)) => (open, trees + 1, lumber)
      case ((open, trees, lumber), (_, Lumberyard)) => (open, trees, lumber + 1)
    }}
  }

  it should "step using local data" in {
    val input =
      """.#.#...|#.
        |.....#|##|
        |.|..|...#.
        |..|#.....#
        |#.#|||#|#|
        |...#.||...
        |.|....|...
        |||...#|.#|
        ||.||||..|.
        |...#.|..|.""".stripMargin

    val chart = Chart.parse(input)

    val input1 =
      """.......##.
        |......|###
        |.|..|...#.
        |..|#||...#
        |..##||.|#|
        |...#||||..
        |||...|||..
        ||||||.||.|
        |||||||||||
        |....||..|.""".stripMargin
    val chart1 = Chart.parse(input1)
    assertResult(chart1)(chart.next)
    assertResult(chart1)(Day18.evolve(chart, 1))

    val input5 =
      """....|||#..
        |...||||#..
        |.|.##||||.
        |..####|||#
        |.|.###||#|
        ||||###||||
        |||||||||||
        |||||||||||
        |||||||||||
        |||||||||||""".stripMargin
    val chart5 = Chart.parse(input5)

    assertResult(chart5)(chart1.next.next.next.next)

    val input10 =
      """.||##.....
        |||###.....
        |||##......
        ||##.....##
        ||##.....##
        ||##....##|
        |||##.####|
        |||#####|||
        |||||#|||||
        |||||||||||""".stripMargin
    val chart10 = Chart.parse(input10)
    assertResult(chart10)(chart5.next.next.next.next.next)

    assertResult(chart10)(Day18.evolve(chart, 10))
  }
}
