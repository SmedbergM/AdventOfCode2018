package day13

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success, Try}

import com.typesafe.scalalogging.LazyLogging
import common.AdventApp
import scalaj.http.Http

object Day13Part1 extends AdventApp with LazyLogging {
  val xy = Day13.firstCollision(Day13.fetchChallenge(sessionId))
  logger.info(s"First collision at ${xy}")
}

object Day13Part2 extends AdventApp with LazyLogging {
  val xy = Day13.lastCartStanding(Day13.fetchChallenge(sessionId))
  logger.info(s"Last cart standing is at ${xy}")
}

object Day13 extends LazyLogging {
  def fetchChallenge(sessionId: String): CartSystem = {
    val url = "https://adventofcode.com/2018/day/13/input"
    val request = Http(url)
      .cookie("session", sessionId)
    val response = request.asString
    CartSystem.parse(response.body)
  }

  @tailrec
  def firstCollision(cartSystem: CartSystem): XY = {
    Try(cartSystem.step) match {
      case Failure(Collision(xy)) => xy
      case Failure(exc) => throw exc
      case Success(nextCartSystem) => firstCollision(nextCartSystem)
    }
  }

  @tailrec
  def lastCartStanding(cartSystem: CartSystem): XY = {
    Try(cartSystem.step) match {
      case Failure(Collision(_)) if cartSystem.carts.size == 3 =>
        val prevWaiting = cartSystem.waitingToMove
        val nextCartSystem = cartSystem.stepAndClearWreckage
        val (xy, cart) :: Nil = nextCartSystem.carts.toList
        if (prevWaiting.contains(xy)) {
          cart.heading.step(xy)
        } else {
          xy
        }
      case Failure(Collision(_)) =>
       lastCartStanding(cartSystem.stepAndClearWreckage)
      case Failure(exc) =>
        throw exc
      case Success(nextCartSystem) => lastCartStanding(nextCartSystem)
    }
  }

  case class XY(x: Int, y: Int)

  case class TrackSystem(tracks: Map[XY, Track]) {
    def isValid: Boolean = tracks.forall { case (xy, track) =>
      CardinalDirection.values.filter(track.connectsTo).forall { direction =>
        tracks.get(direction.step(xy)).exists(other => other.connectsTo(direction.inverse))
      }
    }
  }

  sealed trait Turn {
    def turn(direction: CardinalDirection): (CardinalDirection, Turn)
  }
  case object Left extends Turn {
    override def turn(direction: CardinalDirection): (CardinalDirection, Turn) = {
      val nextDirection = direction match {
        case North => West
        case West => South
        case South => East
        case East => North
      }
      nextDirection -> Straight
    }
  }

  case object Straight extends Turn {
    override def turn(direction: CardinalDirection): (CardinalDirection, Turn) = direction -> Right
  }

  case object Right extends Turn {
    override def turn(direction: CardinalDirection): (CardinalDirection, Turn) = {
      val nextDirection = direction match {
        case North => East
        case East => South
        case South => West
        case West => North
      }
      nextDirection -> Left
    }
  }

  case class Collision(xy: XY) extends Throwable
  class OffTheRails extends Throwable

  case class Cart(heading: CardinalDirection, nextTurn: Turn = Left)

  case class CartSystem(tracks: TrackSystem, carts: Map[XY, Cart], waitingToMove: List[XY]) {
    /*
        assert(waitingToMove.nonEmpty)
        assert(carts.forall{ case (xy, cart) =>
          tracks.tracks.get(xy).exists(track => track.connectsTo(cart.heading))
        })
        assert(waitingToMove.forall(carts.contains))
    */

    def step: CartSystem = {
      def nextCartState(xy: XY): (XY, Cart) = {
        val cart = carts.getOrElse(xy, throw new OffTheRails)
        val nextXY = cart.heading.step(xy)
        if (carts.contains(nextXY)) {
          throw Collision(nextXY)
        } else {
          val nextTrack = tracks.tracks.getOrElse(nextXY, throw new OffTheRails)
          val nextCart = (nextTrack, cart.heading.inverse) match {
            case (NorthSouthTrack | EastWestTrack, _) => cart

            case (SouthEastCurve, South) => cart.copy(heading = East)
            case (SouthEastCurve, East) => cart.copy(heading = South)
            case (SouthEastCurve, _) => throw new OffTheRails

            case (SouthWestCurve, South) => cart.copy(heading = West)
            case (SouthWestCurve, West) => cart.copy(heading = South)
            case (SouthWestCurve, _) => throw new OffTheRails

            case (NorthEastCurve, North) => cart.copy(heading = East)
            case (NorthEastCurve, East) => cart.copy(heading = North)
            case (NorthEastCurve, _) => throw new OffTheRails

            case (NorthWestCurve, North) => cart.copy(heading = West)
            case (NorthWestCurve, West) => cart.copy(heading = North)
            case (NorthWestCurve, _) => throw new OffTheRails

            case (Crossing, _) =>
              val (nextHeading, nextTurn) = cart.nextTurn.turn(cart.heading)
              Cart(nextHeading, nextTurn)
          }
          nextXY -> nextCart
        }
      }
      waitingToMove match {
        case Nil => throw new IllegalStateException()
        case xy :: Nil =>
          val (nextXY, nextCart) = nextCartState(xy)
          val nextCarts = carts - xy + (nextXY -> nextCart)
          CartSystem.from(tracks, nextCarts)
        case xy :: rest =>
          val (nextXY, nextCart) = nextCartState(xy)
          val nextCarts = carts - xy + (nextXY -> nextCart)
          CartSystem(tracks, nextCarts, rest)
      }
    }

    def stepAndClearWreckage: CartSystem = try {
      step
    } catch {
      case Collision(xy) =>
        waitingToMove match {
          case Nil =>
            throw new IllegalStateException()
          case xy1 :: Nil =>
            CartSystem.from(tracks, carts - xy - xy1)
          case xy1 :: rest =>
            val nextCarts = carts - xy - xy1
            CartSystem(tracks, nextCarts, rest.filter(nextCarts.contains))
        }
    }
  }

  object CartSystem {
    def parse(challenge: String): CartSystem = {
      val lines = challenge.split("\n")
      val tracks = TrieMap.empty[XY, Track]
      val carts = TrieMap.empty[XY, Cart]

      def parseLine(line: List[Char], y: Int): Unit = {
        def parseLineIter(cs: List[Char], x: Int): Unit = {
          val xy = XY(x,y)
          cs match {
            case Nil => // then we're done
            case '^' :: rest =>
              tracks.put(xy, NorthSouthTrack)
              carts.put(xy, Cart(North))
              parseLineIter(rest, x + 1)
            case 'v' :: rest =>
              tracks.put(xy, NorthSouthTrack)
              carts.put(xy, Cart(South))
              parseLineIter(rest, x + 1)
            case '>' :: rest =>
              tracks.put(xy, EastWestTrack)
              carts.put(xy, Cart(East))
              parseLineIter(rest, x + 1)
            case '<' :: rest =>
              tracks.put(xy, EastWestTrack)
              carts.put(xy, Cart(West))
              parseLineIter(rest, x + 1)
            case '|' :: rest =>
              tracks.put(xy, NorthSouthTrack)
              parseLineIter(rest, x + 1)
            case '-' :: rest =>
              tracks.put(xy, EastWestTrack)
              parseLineIter(rest, x + 1)
            case '+' :: rest =>
              tracks.put(xy, Crossing)
              parseLineIter(rest, x + 1)
            case '/' :: '<' :: rest =>
              tracks.put(xy, SouthEastCurve)
              parseLineIter('<' :: rest, x + 1)
            case '/' :: '>' :: rest =>
              tracks.put(xy, SouthEastCurve)
              parseLineIter('>' :: rest, x + 1)
            case '/' :: '-' :: rest =>
              tracks.put(xy, SouthEastCurve)
              parseLineIter('-' :: rest, x + 1)
            case '/' :: '+' :: rest =>
              tracks.put(xy, SouthEastCurve)
              parseLineIter('+' :: rest, x + 1)
            case '/' :: rest =>
              tracks.put(xy, NorthWestCurve)
              parseLineIter(rest, x + 1)
            case '\\' :: '<' :: rest =>
              tracks.put(xy, NorthEastCurve)
              parseLineIter('<' :: rest, x + 1)
            case '\\' :: '>' :: rest =>
              tracks.put(xy, NorthEastCurve)
              parseLineIter('>' :: rest, x + 1)
            case '\\' :: '-' :: rest =>
              tracks.put(xy, NorthEastCurve)
              parseLineIter('-' :: rest, x + 1)
            case '\\' :: '+' :: rest =>
              tracks.put(xy, NorthEastCurve)
              parseLineIter('+' :: rest, x + 1)
            case '\\' :: rest =>
              tracks.put(xy, SouthWestCurve)
              parseLineIter(rest, x + 1)
            case c :: rest if c.isWhitespace =>
              parseLineIter(rest, x + 1)
            case c :: rest =>
              logger.warn(s"Unexpected character $c encountered!")
              parseLineIter(rest, x + 1)
          }
        }

        parseLineIter(line, 0)
      }
      lines.zipWithIndex.foreach { case (line, y) =>
        parseLine(line.toList, y)
      }
      val trackSystem = TrackSystem(tracks.toMap)
      CartSystem.from(trackSystem, carts.toMap)
    }


    def nextTick(carts: Map[XY, Cart]): List[XY] = carts.keys.toList.sortWith { case (XY(x1, y1), XY(x2, y2)) =>
      y1 < y2 || (y1 == y2 && x1 < x2)
    }

    def from(trackSystem: TrackSystem, carts: Map[XY, Cart]): CartSystem = CartSystem(trackSystem, carts, nextTick(carts))
  }

  sealed trait CardinalDirection {
    def step(xy: XY): XY
    val inverse: CardinalDirection
  }
  object CardinalDirection {
    val values: Set[CardinalDirection] = Set(North, South, East, West)
  }
  case object North extends CardinalDirection {
    override def step(xy: XY): XY = xy.copy(y = xy.y - 1)

    override val inverse: CardinalDirection = South
  }
  case object South extends CardinalDirection {
    override def step(xy: XY): XY  = xy.copy(y = xy.y + 1)

    override val inverse: CardinalDirection = North
  }
  case object East extends CardinalDirection {
    override def step(xy: XY): XY  = xy.copy(x = xy.x + 1)

    override val inverse: CardinalDirection = West
  }
  case object West extends CardinalDirection {
    override def step(xy: XY): XY  = xy.copy(x = xy.x - 1)

    override val inverse: CardinalDirection = East
  }

  sealed trait Track {
    def connectsTo(direction: CardinalDirection): Boolean
  }
  case object NorthSouthTrack extends Track {
    override def connectsTo(direction: CardinalDirection): Boolean = direction match {
      case North | South => true
      case _ => false
    }
  }

  case object EastWestTrack extends Track {
    override def connectsTo(direction: CardinalDirection): Boolean = direction match {
      case East | West => true
      case _ => false
    }
  }

  case object SouthWestCurve extends Track {
    override def connectsTo(direction: CardinalDirection): Boolean = direction match {
      case South | West => true
      case _ => false
    }
  }

  case object NorthWestCurve extends Track {
    override def connectsTo(direction: CardinalDirection): Boolean = direction match {
      case North | West => true
      case _ => false
    }
  }

  case object NorthEastCurve extends Track {
    override def connectsTo(direction: CardinalDirection): Boolean = direction match {
      case North | East => true
      case _ => false
    }
  }

  case object SouthEastCurve extends Track {
    override def connectsTo(direction: CardinalDirection): Boolean = direction match {
      case South | East => true
      case _ => false
    }
  }

  case object Crossing extends Track {
    override def connectsTo(direction: CardinalDirection): Boolean = true
  }
}

