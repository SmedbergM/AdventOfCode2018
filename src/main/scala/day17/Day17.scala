package day17

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{Queue => MQueue}
import scala.io.Source
import scala.util.matching.Regex
import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import common.AdventApp
import scalaj.http.Http

object Day17Part1 extends AdventApp with LazyLogging {
  //val geologicalSurvey = Day17.fetchChallenge(sessionId)
  val geologicalSurvey = Day17.localChallenge

  val h = geologicalSurvey.hydrate()
  logger.info(s"${h.size} hydrated squares")

  logger.info(s"After the well runs dry, ${h.count(_._2 == Standing)} squares will be hydrated.")
}

object Day17 {
  def localChallenge: GeologicalSurvey = {
    val body = Source.fromInputStream(getClass.getResourceAsStream("/17.challenge")).mkString
    GeologicalSurvey.parse(body)
  }

  def fetchChallenge(sessionId: String): GeologicalSurvey = {
    val url = "https://adventofcode.com/2018/day/17/input"
    val request = Http(url).cookie("session", sessionId).timeout(5000, 5000)
    val response = request.asString
    GeologicalSurvey.parse(response.body)
  }

  def parseInt(s: String): Option[Int] = Try(Integer.parseInt(s)).toOption
}

case class XY(x: Int, y: Int) {
  def left: XY = copy(x = x - 1)
  def right: XY = copy(x = x + 1)
  def up: XY = copy(y = y - 1)
  def down: XY = copy(y = y + 1)
}

sealed trait Water

case object Standing extends Water

case object Flowing extends Water


case class FallFrom(xy: XY)


case class GeologicalSurvey(clays: Set[XY]) extends LazyLogging {
  private val (xMin, xMax) = {
    val xs = clays.map(_.x)
    xs.min -> xs.max
  }
  private val (yMin, yMax) = {
    val ys = clays.map(_.y)
    ys.min -> ys.max
  }

  def draw(upLeft: XY, downRight: XY, waters: Map[XY, Water]): String = {
    def drawLine(y: Int) = (upLeft.x to downRight.x).map { x =>
      val xy = XY(x,y)
      if (clays.contains(xy) && waters.contains(xy)) {
        "?"
      } else if (clays.contains(xy)) {
        "#"
      } else waters.get(xy) match {
        case Some(Standing) => "~"
        case Some(Flowing) => "|"
        case None => "."
      }
    }.mkString
    (upLeft.y to downRight.y).map(drawLine).mkString("\n")
  }

  def hydrate(source: XY = XY(500, 0)): Map[XY, Water] = {
    val waters = TrieMap.empty[XY, Water]
    val queue = MQueue(FallFrom(source))

    def fallFrom(origin: XY): Map[XY, Water] = {
      val fillWaters = TrieMap.empty[XY, Water]

      sealed trait DropResult {
        def fillLeftRight: Map[XY, Water]
      }
      case object NoFill extends DropResult {
        override def fillLeftRight: Map[XY, Water] = Map.empty
      }

      case class NeedsFill(y: Int) extends DropResult {
        override def fillLeftRight: Map[XY, Water] = {
          @tailrec
          def iter(level: Int): Map[XY, Water] = {
            sealed trait Endpoint
            case class WallEndpoint(x: Int) extends Endpoint
            case class FallEndpoint(fallFrom: FallFrom) extends Endpoint
            def blockedBelow(xy: XY): Boolean = {
              val down = xy.down
              waters.get(down).contains(Standing) ||
                fillWaters.get(down).contains(Standing) ||
                clays.contains(down)
            }
            def blocked(xy: XY): Boolean = {
              clays.contains(xy)
            }
            def findEndpoint(next: Int => Int)(x: Int): Endpoint = {
              // invariant: XY(x,y) is never clay
              XY(x,level) match {
                case xy if blockedBelow(xy) && blocked(XY(next(x), level)) =>
                  WallEndpoint(x)
                case xy if blockedBelow(xy) =>
                  findEndpoint(next)(next(x))
                case xy =>
                  FallEndpoint(FallFrom(xy))
              }
            }

            (findEndpoint(_ - 1)(origin.x), findEndpoint(_ + 1)(origin.x)) match {
              case (WallEndpoint(x0), WallEndpoint(x1)) =>
                fillWaters ++= (x0 to x1).map { x =>
                  XY(x,level) -> Standing
                }
                if (clays.contains(origin.copy(y = level - 1))) {
                  // then we've hit the ceiling
                  fillWaters.toMap
                } else {
                  iter(level - 1)
                }
              case (FallEndpoint(ff0), WallEndpoint(x1)) =>
                fillWaters ++= (ff0.xy.x + 1 to x1).map { x =>
                  XY(x,level) -> Flowing
                }
                queue.enqueue(ff0)
                fillWaters.toMap
              case (WallEndpoint(x0), FallEndpoint(ff1)) =>
                fillWaters ++= (x0 until ff1.xy.x).map { x =>
                  XY(x,level) -> Flowing
                }
                queue.enqueue(ff1)
                fillWaters.toMap
              case (FallEndpoint(ff0), FallEndpoint(ff1)) =>
                fillWaters ++= (ff0.xy.x + 1 until ff1.xy.x).map{ x =>
                  XY(x,level) -> Flowing
                }
                queue.enqueue(ff0, ff1)
                fillWaters.toMap
            }
          }
          iter(y)
        }
      }


      if (waters.contains(origin)) {
        Map.empty
      } else {
        @tailrec
        def drop(xy: XY): DropResult = {
          fillWaters.put(xy, Flowing)
          if (xy.y >= yMax) {
            NoFill
          } else if (clays.contains(xy.down)) {
            NeedsFill(xy.y)
          } else waters.get(xy.down) match {
            case Some(Standing) =>
              NeedsFill(xy.y)
            case Some(Flowing) =>
              NoFill
            case None =>
              drop(xy.down)
          }
        }
        val drp = drop(origin)
        val w = fillWaters.toMap ++ drp.fillLeftRight // If I inline this it gives a different answer???
        if (w.nonEmpty) {
          logger.debug(s"Water reached ${w.keys.maxBy(_.y)}")
        }
        w
      }
    }

    def iter(): Map[XY, Water] = queue.dequeueFirst(_ => true) match {
      case None =>
        waters.toMap
      case Some(FallFrom(origin)) if waters.contains(origin) =>
        logger.debug(s"Dequeued ${FallFrom(origin)} which is already wet, discarding...")
        iter()
      case Some(FallFrom(origin)) =>
        logger.debug(s"Dequeued ${FallFrom(origin)}")
        waters ++= fallFrom(origin)
        iter()
    }
    iter().filterKeys { xy =>
      yMin <= xy.y && xy.y <= yMax
    }
  }
}

object GeologicalSurvey {
  def parse(body: String): GeologicalSurvey = {
    val clays = body.split("\n").flatMap(Vein.parse).flatMap(_.xys).toSet
    GeologicalSurvey(clays)
  }
}

sealed trait Vein {
  def xys: Seq[XY]
}

object Vein {
  def parse(s: String): Option[Vein] = HorizontalVein.parse(s).orElse(VerticalVein.parse(s))
}

case class HorizontalVein(y: Int, xMin: Int, xMax: Int) extends Vein {
  override def xys: Seq[XY] = (xMin to xMax).map(x => XY(x,y))
}

object HorizontalVein {
  import Day17.parseInt

  val Pattern: Regex = """y=(\d+), x=(\d+)\.\.(\d+)""".r

  def parse(s: String): Option[HorizontalVein] = s match {
    case Pattern(yy, xx0, xx1) => for {
      y <- parseInt(yy)
      xMin <- parseInt(xx0)
      xMax <- parseInt(xx1)
    } yield HorizontalVein(y, xMin, xMax)
    case _ => None
  }
}

case class VerticalVein(x: Int, yMin: Int, yMax: Int) extends Vein {
  override def xys: Seq[XY] = (yMin to yMax).map(y => XY(x,y))
}

object VerticalVein {
  import Day17.parseInt
  val Pattern: Regex = """x=(\d+), y=(\d+)\.\.(\d+)""".r

  def parse(s: String): Option[VerticalVein] = s match {
    case Pattern(xx, yy0, yy1) => for {
      x <- parseInt(xx)
      yMin <- parseInt(yy0)
      yMax <- parseInt(yy1)
    } yield VerticalVein(x, yMin, yMax)
    case _ => None
  }
}