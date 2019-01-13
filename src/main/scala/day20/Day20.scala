package day20

import scala.collection.concurrent.TrieMap
import scala.io.Source

import com.typesafe.scalalogging.LazyLogging
import scalaj.http.Http

object Day20Part1 extends App with LazyLogging {
  val path = Day20.localChallenge

  logger.info(s"Path depth: ${path.depth}")
}

object Day20 {
  def fetchChallenge(sessionId: String): ForkedPath = {
    val url = "https://adventofcode.com/2018/day/20/input"
    val request = Http(url)
      .cookie("session", sessionId)
    ForkedPath.parse(request.asString.body.trim)
  }

  def localChallenge: ForkedPath = {
    val challenge = Source.fromInputStream(getClass.getResourceAsStream("/20.challenge")).mkString
    ForkedPath.parse(challenge)
  }
}

case class FloorPlan(squares: Map[XY, GridSquare])

object FloorPlan extends LazyLogging {
  def build(forkedPath: ForkedPath): FloorPlan = {
    val squares = TrieMap.empty[XY, GridSquare]

    def iter(point: XY, path: ForkedPath, forkPoints: List[(XY, ForkedPath.Fork)]): Unit = {
      squares.put(point, GridSquare.Room)
      (path, forkPoints) match {
        case (ForkedPath.Empty, Nil) =>
          logger.info("Traversal complete!")
        case (ForkedPath.Empty, (xy, ForkedPath.Fork(Nil, tail)) :: rest) =>
          iter(xy, tail, rest)
        case (ForkedPath.Empty, (xy, ForkedPath.Fork(f0 :: forks, tail)) :: rest) =>
          iter(xy, f0, (xy -> ForkedPath.Fork(forks, tail)) :: rest)

        case (ForkedPath.OneWay(head, tail), _) =>
          val nextPoint = head match {
            case Cardinal.North | Cardinal.South =>
              val door = point.step(head)
              squares.put(door, GridSquare.NSDoor)
              door.step(head)
            case Cardinal.East | Cardinal.West =>
              val door = point.step(head)
              squares.put(door, GridSquare.EWDoor)
              door.step(head)
          }
          iter(nextPoint, tail, forkPoints)

        case (ForkedPath.Fork(Nil, tail), _) =>
          iter(point, tail, forkPoints)
        case (ForkedPath.Fork(f0 :: forks, tail), _) =>
          iter(point, f0, (point -> ForkedPath.Fork(forks, tail)) :: forkPoints)
      }
    }

    iter(XY(0,0), forkedPath, Nil)

    FloorPlan(squares.toMap)
  }

  private implicit class RichInt(x: Int) {
    def isEven: Boolean = x % 2 == 0
  }
}

case class XY(x: Int, y: Int) {
  def step(cardinal: Cardinal): XY = cardinal match {
    case Cardinal.North => copy(y = y - 1)
    case Cardinal.South => copy(y = y + 1)
    case Cardinal.West => copy(x = x - 1)
    case Cardinal.East => copy(x = x + 1)
  }
}

sealed trait GridSquare

object GridSquare {
  case object Wall extends GridSquare {
    override def toString: String = "#"
  }
  case object Room extends GridSquare {
    override def toString: String = "."
  }
  case object NSDoor extends GridSquare {
    override def toString: String = "-"
  }
  case object EWDoor extends GridSquare {
    override def toString: String = "|"
  }
}

sealed trait ForkedPath {
  def prepend(cardinal: Cardinal): ForkedPath
  val depth: Int
}

object ForkedPath {
  case object Empty extends ForkedPath {
    override def prepend(cardinal: Cardinal): ForkedPath = OneWay(cardinal, Empty)
    override val depth: Int = 0

    override def toString: String = "⊣"
  }

  case class OneWay(head: Cardinal, tail: ForkedPath) extends ForkedPath {
    override def prepend(cardinal: Cardinal): ForkedPath = OneWay(cardinal, tail.prepend(head))
    override val depth: Int = 1 + tail.depth

    override def toString: String = s"${head}${tail}"
  }

  case class Fork(forks: List[ForkedPath], tail: ForkedPath) extends ForkedPath {
    override def prepend(cardinal: Cardinal): ForkedPath = OneWay(cardinal, this)
    override val depth: Int = 1 + forks.foldLeft(tail.depth){case (a, fp) => a.max(fp.depth)}

    override def toString: String = s"(${forks.mkString("|")})${tail}"
  }

  def parse(s: String): ForkedPath = {
    sealed trait ParseState {
      def getPath: ForkedPath
      def prepend(cardinal: Cardinal): ParseState
    }
    case object Initial extends ParseState {
      override def getPath: ForkedPath = Empty
      override def prepend(cardinal: Cardinal): ParseState = Failed
    }
    case object Failed extends ParseState {
      override def getPath: ForkedPath = throw new IllegalStateException()
      override def prepend(cardinal: Cardinal): ParseState = Failed
    }
    case class Succeeded(path: ForkedPath) extends ParseState {
      override def getPath: ForkedPath = path

      override def prepend(cardinal: Cardinal): ParseState = throw new IllegalStateException()
    }
    case class TopLevel(tail: ForkedPath) extends ParseState {
      override def getPath: ForkedPath = tail
      override def prepend(cardinal: Cardinal): ParseState = TopLevel(tail.prepend(cardinal))
    }
    sealed trait BuildFork extends ParseState {
      def closeAlternative: ParseState
      def closeChild(h: ForkedPath, ps: List[ForkedPath]): BuildFork
    }
    case class BuildTopFork(current: ForkedPath, built: List[ForkedPath], tail: ForkedPath) extends BuildFork {
      override def getPath: ForkedPath = throw new IllegalStateException()

      override def prepend(cardinal: Cardinal): ParseState = this.copy(current = current.prepend(cardinal))

      override def closeAlternative: ParseState = this.copy(
        current = Empty,
        built = current :: built
      )

      override def closeChild(h: ForkedPath, ps: List[ForkedPath]): BuildFork = {
        this.copy(current = Fork(h :: ps, current))
      }
    }
    case class BuildInnerFork(current: ForkedPath, built: List[ForkedPath], parent: BuildFork) extends BuildFork {
      override def getPath: ForkedPath = throw new IllegalStateException()

      override def prepend(cardinal: Cardinal): ParseState = this.copy(current = current.prepend(cardinal))

      override def closeAlternative: ParseState = this.copy(
        current = Empty,
        built = current :: built
      )

      override def closeChild(h: ForkedPath, ps: List[ForkedPath]): BuildFork = {
        this.copy(current = Fork(h :: ps, current))
      }
    }

    s.foldRight[ParseState](Initial){
      case ('$', Initial) => TopLevel(Empty)
      case ('N', state) => state.prepend(Cardinal.North)
      case ('S', state) => state.prepend(Cardinal.South)
      case ('E', state) => state.prepend(Cardinal.East)
      case ('W', state) => state.prepend(Cardinal.West)

      case (')', TopLevel(tail)) => BuildTopFork(Empty, Nil, tail)
      case (')', bf: BuildFork) => BuildInnerFork(Empty, Nil, bf)

      case ('(', TopLevel(_)) => Failed
      case ('(', BuildTopFork(current, built, tail)) =>
        TopLevel(Fork(current :: built, tail))
      case ('(', BuildInnerFork(current, built, parent)) =>
        parent.closeChild(current, built)

      case ('|', bf: BuildFork) =>
        bf.closeAlternative

      case ('^', TopLevel(tail)) =>
        Succeeded(tail)
      case _ =>
        Failed
    }.getPath
  }
}

sealed trait Cardinal

object Cardinal {
  case object North extends Cardinal {
    override def toString: String = "N"
  }
  case object West extends Cardinal {
    override def toString: String = "W"
  }
  case object East extends Cardinal {
    override def toString: String = "E"
  }
  case object South extends Cardinal {
    override def toString: String = "S"
  }
}

