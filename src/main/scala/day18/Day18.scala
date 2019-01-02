package day18

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.io.Source

import com.typesafe.scalalogging.LazyLogging
import scalaj.http.Http

object Day18Part1 extends App with LazyLogging {
  val chart = Day18.localChallenge()

  logger.info(s"Resource value of challenge: ${Day18.evolve(chart, 10).resourceValue}")
}

object Day18Part2 extends App with LazyLogging {
  val total = BigInt("1000").pow(3)

  logger.info(s"The resource value of the billionth iteration is ${Day18.evolve(Day18.localChallenge(), total).resourceValue}")
}

object Day18 {
  def localChallenge(): Chart = {
    val body = Source.fromInputStream(getClass.getResourceAsStream("/18.challenge")).mkString
    Chart.parse(body)
  }

  def fetchChallenge(sessionId: String): Chart = {
    val url = "https://adventofcode.com/2018/day/18/input"
    val request = Http(url).cookie("session", sessionId)
    val response = request.asString
    Chart.parse(response.body)
  }

  def evolve(initial: Chart, iterations: BigInt): Chart = {
    val charts = TrieMap.empty[Chart, BigInt]

    @tailrec
    def iter(remaining: BigInt, chart: Chart): Chart = charts.get(chart) match {
      case _ if remaining == 0 => chart
      case None =>
        charts.put(chart, iterations - remaining)
        iter(remaining - 1, chart.next)
      case Some(prevIdx) =>
        val period = iterations - remaining - prevIdx
        val mod = remaining % period
        BigInt(0).until(mod).foldLeft(chart)((c, _) => c.next)
    }
    iter(iterations, initial)
  }
}

case class Chart(acres: Map[XY, Acre]) {

  def resourceValue: Int = {
    val (nTrees, nLumber) = acres.foldLeft((0,0)){
      case ((nt,nl), (_, Trees)) => (nt + 1, nl)
      case ((nt,nl), (_, Lumberyard)) => (nt, nl + 1)
      case (acc, _) => acc
    }
    nTrees * nLumber
  }

  def next: Chart = {
    val nextAcres = acres.map {
      case (xy, Open) if xy.neighbors.count(nbr => acres.get(nbr).contains(Trees)) >= 3 =>
        xy -> Trees
      case (xy, Open) =>
        xy -> Open
      case (xy, Trees) if xy.neighbors.count(nbr => acres.get(nbr).contains(Lumberyard)) >= 3 =>
        xy -> Lumberyard
      case (xy, Trees) =>
        xy -> Trees
      case (xy, Lumberyard)
        if xy.neighbors.exists(nbr => acres.get(nbr).contains(Lumberyard)) &&
          xy.neighbors.exists(nbr => acres.get(nbr).contains(Trees)) =>
        xy -> Lumberyard
      case (xy, Lumberyard) =>
        xy -> Open

    }
    Chart(nextAcres)
  }
}

object Chart extends LazyLogging {
  def parse(body: String): Chart = {
    def parseLine(line: String, y: Int): Map[XY, Acre] = {
      line.zipWithIndex.foldLeft(Map.empty[XY, Acre]){
        case (acc, ('.', x)) => acc + (XY(x,y) -> Open)
        case (acc, ('|', x)) => acc + (XY(x,y) -> Trees)
        case (acc, ('#', x)) => acc + (XY(x,y) -> Lumberyard)
        case (acc, (c, _)) =>
          logger.warn(s"Unexpected character ${c} encountered...")
          acc
      }
    }

    val acres = body.split("\n").zipWithIndex.flatMap { case (line, idx) =>
      parseLine(line, idx)
    }.toMap
    Chart(acres)
  }
}

sealed trait Acre

case object Lumberyard extends Acre
case object Trees extends Acre
case object Open extends Acre

case class XY(x: Int, y: Int) {
  def north: XY = copy(y = y - 1)
  def west: XY = copy(x = x - 1)
  def east: XY = copy(x = x + 1)
  def south: XY = copy(y = y + 1)

  def neighbors: Iterable[XY] = Iterable(
    north.west, north, north.east,
    west, east,
    south.west, south, south.east
  )
}