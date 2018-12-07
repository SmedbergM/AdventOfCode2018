package day6

import java.time.{Duration, Instant}
import java.util.concurrent.atomic.AtomicReference
import java.util.function.UnaryOperator
import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import common.AdventApp
import scalaj.http.Http

object Day6Part1 extends AdventApp {
  val challenge = Day6.fetchChallenge(sessionId)

  println(s"Task 1: ${Day6.task1(challenge)}")
}

object Day6Part2 extends AdventApp {
  val challenge = Day6.fetchChallenge(sessionId)

  val startTime = Instant.now
  println(s"Task 2: ${Day6.task2(challenge)}")
  println(s"Task 2 took ${Duration.between(startTime, Instant.now).toMillis} ms")
}

object Day6 extends {
  type Location = (Int, Int)
  def fetchChallenge(sessionId: String): Seq[Location] = {
    val url = "https://adventofcode.com/2018/day/6/input"
    val request = Http(url)
      .cookie("session", sessionId)
    val response = request.asString
    val pat = """(\d+),\s*(\d+)""".r

    response.body.split("\n").flatMap {
      case pat(xx, yy) => for {
        x <- parseInt(xx)
        y <- parseInt(yy)
      } yield (x,y)
    }.toVector
  }

  def task1(locations: Seq[Location]): Option[Int] = {
    BoundingBox.from(locations).map { boundingBox =>
      val gridSquares = (boundingBox.xmin to boundingBox.xmax).flatMap { x =>
        (boundingBox.ymin to boundingBox.ymax).map { y =>
          (x,y) -> new GridSquare((x,y))
        }
      }.toMap

      locations.par.foreach { xy =>
        gridSquares.par.foreach { case (loc, gs) =>
          gs.visit(xy)
        }
      }

      // Border locations, those with infinite supervision area, are those occurring on the border
      val bbLocations: Set[Location] = boundingBox.frontier.flatMap { xy =>
        gridSquares.get(xy).toIterable.flatMap { gs =>
          gs.nearestAllowTies
        }
      }.toSet

      gridSquares.foldLeft(Map.empty[(Int, Int), Int]) {
        case (acc, (loc, _)) if bbLocations.contains(loc) => acc
        case (acc, (_, gs)) =>
          gs.nearest match {
            case None =>
              acc
            case Some(t) =>
              acc + (t -> (acc.getOrElse(t, 0) + 1))
          }
      }.values.max
    }
  }

  def task2(locations: Seq[Location]): Option[Int] = {
    BoundingBox.from(locations).flatMap { boundingBox =>
      locationMean(locations).map { locMean =>
        def task2Iter(acc: Int, circles: Stream[Set[Location]]): Int = circles match {
          case Stream.Empty => acc
          case circle #:: rest =>
            val c = circle.par.flatMap { gs =>
              val aggDistance = locations.par.map(_.distance(gs)).sum
              Some(gs).filter(_ => aggDistance < 10000)
            }.size
            if (c > 0 || circle.forall(boundingBox.contains)) {
              task2Iter(acc + c, rest)
            } else {
              acc + c
            }
        }

        task2Iter(0, manhattanCircles(locMean))
      }
    }
  }

  private def manhattanCircles(origin: Location): Stream[Set[Location]] = {
    Set(origin) #:: Stream.from(1).map { d =>
      //Set.range(d, -d, -1).map(x => x -> (d - math.abs(x))) #::: Set.range(-d, d, 1).map(x => x -> (math.abs(x) - d))
      (d until -d by -1).map(x => origin + (x -> (d - math.abs(x)))).toSet ++ (-d until d).map(x => origin + (x -> (math.abs(x) - d)))
    }
  }

  private def locationMean(locations: Iterable[Location]): Option[Location] = {
    val (optSum, count) = locations.foldLeft(Option.empty[(Double, Double)], 0) {
      case ((optAcc, c), (x,y)) => optAcc.orElse(Some(0.0,0.0)).map { case (xAcc, yAcc) =>
        (xAcc + x, yAcc + y)
      } -> (c + 1)
    }
    optSum.map { case (xSum, ySum) =>
      (xSum / count).intValue() -> (ySum / count).intValue()
    }
  }

  private case class BoundingBox(xmin: Int, xmax: Int, ymin: Int, ymax: Int) {

    def contains(loc: Location): Boolean = {
      xmin <= loc._1 && loc._1 <= xmax && ymin <= loc._2 && loc._2 <= ymax
    }

    def frontier: Iterable[Location] = {
      (xmax until xmax).map(_ -> ymin) ++
        (ymin until ymax).map(xmax -> _) ++
        (xmax until xmin by -1).map(_ -> ymax) ++
        (ymax until ymin by -1).map(xmin -> _)
    }

  }
  private object BoundingBox {
    def from(locations: Iterable[Location]): Option[BoundingBox] = locations.foldLeft(Option.empty[BoundingBox]){ case (optBB, (x,y)) => optBB.map { bb =>
      bb.copy(
        xmin = bb.xmin.min(x),
        xmax = bb.xmax.max(x),
        ymin = bb.ymin.min(y),
        ymax = bb.ymax.max(y)
      )}.orElse(Option(BoundingBox(x ,x ,y ,y)))
    }.map { bb =>
      bb.copy(
        xmin = bb.xmin - 1,
        xmax = bb.xmax + 1,
        ymin = bb.ymin - 1,
        ymax = bb.ymax + 1
      )
    }
  }

  private def parseInt(s: String): Option[Int] = Try(Integer.parseInt(s)).toOption
}

class GridSquare(xy: Day6.Location) extends LazyLogging {
  private val nearestLocations: AtomicReference[List[Day6.Location]] = new AtomicReference(Nil)
  // invariant: All locations in this list are equidistant from xy

  def visit(visitor: Day6.Location): Unit = {
    val visitorDistance = visitor.distance(xy)
    object Updater extends UnaryOperator[List[Day6.Location]] {
      override def apply(prevs: List[(Int, Int)]): List[(Int, Int)] = {
        prevs.headOption.map(_.distance(xy)) match {
          case None =>
            visitor :: Nil
          case Some(prevDistance) if prevDistance < visitorDistance =>
            prevs
          case Some(prevDistance) if prevDistance == visitorDistance =>
            visitor :: prevs
          case Some(prevDistance) if visitorDistance < prevDistance =>
            visitor :: Nil
        }
      }
    }
    nearestLocations.getAndUpdate(Updater)
  }

  def nearestAllowTies: List[Day6.Location] = nearestLocations.get

  def nearest: Option[Day6.Location] = nearestLocations.get() match {
    case Nil =>
      logger.warn(s"Grid square ${xy} never visited!")
      None
    case loc :: Nil => Some(loc)
    case _ =>
      None
  }
}