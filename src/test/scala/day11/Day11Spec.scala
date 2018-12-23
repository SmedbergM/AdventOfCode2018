package day11

import java.util.concurrent.ThreadLocalRandom
import scala.util.Random

import scala.collection.JavaConverters._
import org.scalatest.FlatSpec

class Day11Spec extends FlatSpec {
  "hundredsDigit" should "extract the hundreds digit" in {
    assertResult(0)(Day11.hundredsDigit(0))
    assertResult(0)(Day11.hundredsDigit(1))
    assertResult(0)(Day11.hundredsDigit(-1))

    assertResult(1)(Day11.hundredsDigit(100))
    assertResult(1)(Day11.hundredsDigit(101))
    assertResult(1){
      val du = Random.nextInt(100)
      Day11.hundredsDigit(100 + du)
    }

    assertResult(2)(Day11.hundredsDigit(5280))
    assertResult(2)(Day11.hundredsDigit(-5280))

    val tlr = ThreadLocalRandom.current()
    val intStream = tlr.ints(0,10)
      .iterator().asScala.toStream
    val dm #:: m #:: c #:: d #:: u #:: _ = intStream
    val x = 10000*dm + 1000*m + 100*c + 10*d + u
    assertResult(c)(Day11.hundredsDigit(x))
    assertResult(c)(Day11.hundredsDigit(-x))
  }

  "FuelCell.powerLevel" should "compute a power level" in {
    val fuelCell = Day11.FuelCell(3,5)
    assertResult(4)(fuelCell.powerLevel(8))

    val fuelCell1 = Day11.FuelCell(122,79)
    assertResult(-5)(fuelCell1.powerLevel(57))

    val fuelCell2 = Day11.FuelCell(217,196)
    assertResult(0)(fuelCell2.powerLevel(39))

    val fuelCell3 = Day11.FuelCell(101,153)
    assertResult(4)(fuelCell3.powerLevel(71))
  }

  "part1" should "maximize 3x3 power" in {
    import Day11.FuelCell
    assertResult(Some(FuelCell(33, 45) -> 29))(Day11.part1(18))

    assertResult(Some(FuelCell(21,61) -> 30))(Day11.part1(42))
  }

  "part2" should "maximize NxN power" in {
    import Day11.{FuelSquare, FuelCell}
    assertResult(Some(FuelSquare(FuelCell(90,269),16) -> 113))(Day11.part2(18))
  }
}
