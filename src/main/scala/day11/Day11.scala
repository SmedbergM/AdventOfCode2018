package day11

import scala.annotation.tailrec

import com.typesafe.scalalogging.LazyLogging


object Day11Part1 extends App {
  val serialNumber = Day11.serialNumber

  println(s"Maximum 3x3 power: ${Day11.part1(serialNumber)}")
}

object Day11Part2 extends App {
  val serialNumber = Day11.serialNumber

  Day11.part2(serialNumber)
}

object Day11 extends LazyLogging {

  def serialNumber: Int = Integer.parseInt(sys.env("DAY_11_SERIAL_NUMBER"))

  case class FuelCell(x: Int, y: Int) {
    def rackId: Int = x + 10

    def powerLevel(serialNumber: Int): Int = {
      hundredsDigit(((rackId * y) + serialNumber) * rackId) - 5
    }
  }

  def hundredsDigit(x: Int): Int = {
    (math.abs(x) / 100) % 10
  }

  def part1(serialNumber: Int, xMax: Int = 300, yMax: Int = 300): Option[(FuelCell, Int)] = {
    def power3x3(fuelCell: FuelCell): Int = (for {
      x <- fuelCell.x until fuelCell.x + 3
      y <- fuelCell.y until fuelCell.y + 3
    } yield FuelCell(x,y).powerLevel(serialNumber)).sum

    (1 to xMax - 2).flatMap { x =>
      (1 to yMax - 2).map(x -> _)
    }.foldLeft(Option.empty[(FuelCell, Int)]) {
      case (None, (x0, y0)) =>
        val fuelCell = FuelCell(x0, y0)
        Option(fuelCell -> power3x3(fuelCell))
      case (Some((fuelCell, best)), (x0, y0)) =>
        val nextFuelCell = FuelCell(x0, y0)
        val nextBest = power3x3(nextFuelCell)
        if (nextBest > best) {
          Option(nextFuelCell -> nextBest)
        } else {
          Option(fuelCell, best)
        }
    }
  }

  case class FuelSquare(fuelCell: FuelCell, size: Int)

  def part2(serialNumber: Int, xMax: Int = 300, ymax: Int = 300): Option[(FuelSquare, Int)] = {
    val fuelCells = (for {
      x <- 1 to xMax - 2
      y <- 1 to ymax - 2
      fuelCell = FuelCell(x,y)
    } yield fuelCell -> fuelCell.powerLevel(serialNumber)).toMap

    val best1 = fuelCells.foldLeft(Option.empty[(FuelSquare, Int)]) {
      case (None, (fuelCell, powerLevel)) => Some(FuelSquare(fuelCell, 1) -> powerLevel)
      case (Some((_, bestPowerLevel)), (fuelCell, powerLevel)) if powerLevel > bestPowerLevel =>
        Some(FuelSquare(fuelCell, 1), powerLevel)
      case (acc, _) => acc
    }
    def fuelSquares1 = fuelCells.map { case (fuelCell, powerLevel) =>
      FuelSquare(fuelCell, 1) -> powerLevel
    }

    @tailrec
    def part2Iter(best: Option[(FuelSquare, Int)], previousLevel: Map[FuelSquare, Int]): Option[(FuelSquare, Int)] = {
      logger.info(s"Best so far: ${best}")
      logger.info(s"Previous level contained ${previousLevel.size} squares of size ${previousLevel.headOption.map(_._1.size)}")
      val nextLevel: Map[FuelSquare, Int] = previousLevel.flatMap { case (fuelSquare, power) =>
        val (nextX, nextY) = (fuelSquare.fuelCell.x - 1, fuelSquare.fuelCell.y - 1)
        val nextFuelCell = fuelSquare.fuelCell.copy(x = nextX, y = nextY)
        fuelCells.get(nextFuelCell).map { powerLevel =>
          val frontierCells = for {
            k <- 1 to fuelSquare.size
            fuelCell <- List(FuelCell(nextX, nextY + k), FuelCell(nextX + k, nextY))
          } yield fuelCell
          val nextPowerLevel = frontierCells.foldLeft(power + powerLevel) { case (acc, fuelCell) =>
            acc + fuelCells.getOrElse(fuelCell,0)
          }
          FuelSquare(nextFuelCell, fuelSquare.size + 1) -> nextPowerLevel
        }
      }
      if (nextLevel.isEmpty) {
        best
      } else {
        val nextBest = nextLevel.foldLeft(best) {
          case (None, (fuelSquare, power)) => Some(fuelSquare -> power)
          case (Some((_, bestPower)), (fuelSquare, power)) if power > bestPower => Some(fuelSquare -> power)
          case (acc, _) => acc
        }
        part2Iter(nextBest, nextLevel)
      }
    }

    part2Iter(best1, fuelSquares1)
  }
}


