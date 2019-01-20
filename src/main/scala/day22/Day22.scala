package day22

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.io.Source
import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import scalaj.http.Http

object Day22Part1 extends App with LazyLogging {
  val cave = Cave.parse(Day22.localRawChallenge).get

  logger.info(s"Total risk: ${cave.aggregateRisk}")
}

object Day22Part2 extends App with LazyLogging {
  val cave = Cave.parse(Day22.localRawChallenge).get


  val b = cave.fastestPathToTarget()
  logger.info(s"Shortest path length: ${b}")
}

object Day22 {
  def fetchRawChallenge(sessionId: String): String = {
    val url = "https://adventofcode.com/2018/day/22/input"
    val request = Http(url)
      .cookie("session", sessionId)
    request.asString.body
  }

  def localRawChallenge: String = {
    Source.fromInputStream(getClass.getResourceAsStream("/22.challenge")).mkString
  }

}

case class XY(x: Int, y: Int) {
  def left: XY = copy(x = x - 1)
  def right: XY = copy(x = x + 1)
  def up: XY = copy(y = y - 1)
  def down: XY = copy(y = y + 1)

  def neighbors: Iterable[XY] = Iterable(left, right, up, down).filter{
    xy => xy.x >= 0 && xy.y >= 0
  }
}

sealed trait GeoType {
  val equipments: Set[Equipment]
}

object GeoType {
  def of(j: Int): GeoType = j match {
    case 0 => Rocky
    case 1 => Wet
    case 2 => Narrow
    case _ => throw new IllegalArgumentException
  }

  case object Rocky extends GeoType {
    override val equipments: Set[Equipment] = Set(Equipment.ClimbingGear, Equipment.Light)
  }
  case object Wet extends GeoType {
    override val equipments: Set[Equipment] = Set(Equipment.ClimbingGear, Equipment.Neither)
  }
  case object Narrow extends GeoType {
    override val equipments: Set[Equipment] = Set(Equipment.Light, Equipment.Neither)
  }
}

sealed trait Equipment

object Equipment {
  case object Neither extends Equipment
  case object ClimbingGear extends Equipment
  case object Light extends Equipment
}

class Cave(val depth: Int, val target: XY) extends LazyLogging {
  private type GeologicIndex = Int
  private type ErosionLevel = Int


  private val geologicIndices = TrieMap.empty[XY, GeologicIndex]

  def geologicIndex(xy: XY): GeologicIndex = geologicIndices.getOrElseUpdate(xy, xy match {
    case XY(0,0) => 0
    case XY(x,y) if x < 0 || y < 0 =>
      throw new IllegalArgumentException
    case _ if xy == target => 0
    case XY(x,0) => x * Cave.X_AXIS_MULTIPLIER
    case XY(0,y) => y * Cave.Y_AXIS_MULTIPLIER
    case _ =>
      erosionLevel(xy.left) * erosionLevel(xy.up)
  })

  private val erosionLevels = TrieMap.empty[XY, ErosionLevel]
  def erosionLevel(xy: XY): ErosionLevel = erosionLevels
    .getOrElseUpdate(xy, (geologicIndex(xy) + depth) % Cave.MAGIC_MODULUS)

  def risk(xy: XY): Int = erosionLevel(xy) % 3

  def geoType(xy: XY): GeoType = GeoType.of(erosionLevel(xy) % 3)

  def pretty(xMax: Int = target.x): String = {
    (0 to target.y).map { y =>
      (0 to xMax).map { x =>
        geoType(XY(x,y)) match {
          case GeoType.Rocky => "."
          case GeoType.Wet => "="
          case GeoType.Narrow => "|"
        }
      }.mkString
    }.mkString("\n")
  }

  def aggregateRisk: Int = (for {
    x <- 0 to target.x
    y <- 0 to target.y
  } yield risk(XY(x,y))).sum

  def fastestPathToTarget(): Int = {
    val visited = TrieMap.empty[(XY, Equipment), Int]
    val queue = new GradedQueue[(XY, Equipment)]
    queue.put((XY(0,0), Equipment.Light), 0)

    def enqueueNeighbors(xy: XY, equipment: Equipment, dist: Int): Unit = xy.neighbors.foreach { nbr =>
      geoType(xy).equipments.intersect(geoType(nbr).equipments).collect {
        case nextEquipment if nextEquipment == equipment
          && visited.get(nbr -> nextEquipment).forall(_ > dist + 1) =>

          queue.put((nbr, nextEquipment), dist + 1)
        case nextEquipment if visited.get(nbr -> nextEquipment).forall(_ > dist + 8) =>
          queue.put((nbr, nextEquipment), dist + 8)
      }
    }

    @tailrec
    def iter(): Int = queue.pollOption() match {
      case None =>
        throw new IllegalStateException()
      case Some(((xy, Equipment.Light), d)) if xy == target =>
        d
      case Some(((xy, _), d)) if xy == target =>
        queue.put((target, Equipment.Light), d + 7)
        iter()
      case Some(((xy, equipment), d)) => visited.get(xy -> equipment) match {
        case Some(prevD) if prevD <= d =>
          iter()
        case _ =>
          visited.put(xy -> equipment, d )
          enqueueNeighbors(xy, equipment, d)
          iter()
      }
    }

    iter()
  }
}

object Cave {
  def parse(s: String): Option[Cave] = {
    val depthPat = """depth:\s*(\d+)""".r
    val targetPat = """target:\s*(\d+),(\d+)""".r

    val (optDepth, optTarget) = s.split("\n").foldLeft(Option.empty[Int] -> Option.empty[XY]){
      case ((None, optXY), depthPat(d)) => Try(d.toInt).toOption -> optXY
      case ((optD, None), targetPat(tx, ty)) =>
        val optXY = for {
          x <- Try(tx.toInt).toOption
          y <- Try(ty.toInt).toOption
        } yield XY(x,y)
        optD -> optXY
      case (acc, _) => acc
    }
    for {
      depth <- optDepth
      target <- optTarget
    } yield new Cave(depth, target)
  }

  private val X_AXIS_MULTIPLIER = 16807
  private val Y_AXIS_MULTIPLIER = 48271
  private val MAGIC_MODULUS = 20183

}

class GradedQueue[T] extends LazyLogging {
  import java.util.concurrent.{ConcurrentLinkedQueue => CQueue}

  def put(t: T, grade: Int): Unit = {
    queues.getOrElseUpdate(grade, new CQueue[T]).add(t)
  }
  def pollOption(): Option[(T, Int)] = {
    def iter(): Option[(T, Int)] = {
      try {
        val (k,q) = queues.minBy(_._1)
        if (q.isEmpty) {
          queues.remove(k)
          iter()
        } else {
          Option(q.poll() -> k)
        }
      } catch {
        case _: UnsupportedOperationException => // minBy on empty collection
          None
      }
    }

    iter()
  }

  private val queues = TrieMap.empty[Int, CQueue[T]]
}