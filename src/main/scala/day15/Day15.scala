package day15

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{HashSet => MHashSet, Queue => MQueue}
import scala.util.{Failure, Success, Try}

import com.typesafe.scalalogging.LazyLogging
import common.AdventApp
import scalaj.http.Http

object Day15Part1 extends AdventApp with LazyLogging {
  val board = Day15.fetchChallenge(sessionId)
  logger.info(s"Task 1: ${Day15.task1(board)}")
}

object Day15Part2 extends AdventApp with LazyLogging {
  val board = Day15.fetchChallenge(sessionId)
  val result = Day15.task2(board)
  logger.info(s"Task 2: ${result}")
}

object Day15 extends LazyLogging {
  def fetchChallenge(sessionId: String): BattleBoard = {
    val url = "https://adventofcode.com/2018/day/15/input"
    val request = Http(url).cookie("session", sessionId)
    val response = request.asString
    BattleBoard.parse(response.body)
  }

  def task1(battleBoard: BattleBoard): Int = {
    @tailrec
    def iter(board: BattleBoard, fullRoundsCompleted: Int): Int = {
      Try {
        val nextBoard = board.takeTurn
        val nextFRC = if (nextBoard.battleUnits.size == nextBoard.awaitingTurn.size) {
          logger.debug(s"Completing round ${fullRoundsCompleted + 1}")
          fullRoundsCompleted + 1
        } else fullRoundsCompleted
        nextBoard -> nextFRC
      } match {
        case Success((nextBoard, nextFRC)) =>
          iter(nextBoard, nextFRC)
        case Failure(CombatComplete) =>
          fullRoundsCompleted * board.battleUnits.values.map(_.hitPoints).sum
        case Failure(exc) =>
          throw exc
      }
    }

    iter(battleBoard, 0)
  }

  def task2(battleBoard: BattleBoard): Int = {
    @tailrec
    def iter(board: BattleBoard, elfAttackPower: Int, fullRoundsCompleted: Int): Int = {
      def nextFRC(nextBoard: BattleBoard): Int = if (nextBoard.battleUnits.size == nextBoard.awaitingTurn.size) {
        logger.debug(s"Completed ${fullRoundsCompleted} rounds without an elf fatality.")
        fullRoundsCompleted + 1
      } else fullRoundsCompleted

      Try {
        val moveResult = board.move()
        val battleResult = board.doBattle(moveResult)
        (moveResult, battleResult) match {
          case (_, NoBattle | Damage(_, _)) =>
            val nextBoard = board.finalize(moveResult, battleResult)
            (nextBoard, elfAttackPower, nextFRC(nextBoard))
          case (AdjacentToEnemy(_, Elf(_,_), _) | Moved(_, Elf(_,_)), Kill(_)) =>
            val nextBoard = board.finalize(moveResult, battleResult)
            (nextBoard, elfAttackPower, nextFRC(nextBoard))
          case (AdjacentToEnemy(_, Goblin(_), _) | Moved(_, Goblin(_)), Kill(_)) =>
            val nextElfAttackPower = elfAttackPower + 1
            logger.info(s"First elf fatality with attack power ${elfAttackPower} after ${fullRoundsCompleted} rounds.")
            val nextBattleUnits = battleBoard.battleUnits.map {
              case (xy, g: Goblin) => xy -> g
              case (xy, e: Elf) => xy -> e.copy(attackPower = nextElfAttackPower)
            }
            val nextBoard = battleBoard.copy(battleUnits = nextBattleUnits)
            (nextBoard, nextElfAttackPower, 0)
          case (_, Kill(_)) =>
            throw new IllegalStateException()
        }

      } match {
        case Success((nextBoard, nextElfAttackPower, nextFullRoundsCompleted)) =>
          iter(nextBoard, nextElfAttackPower, nextFullRoundsCompleted)
        case Failure(CombatComplete) =>
          fullRoundsCompleted * board.battleUnits.values.map(_.hitPoints).sum
        case Failure(exc) =>
          throw exc
      }

    }

    iter(battleBoard, 4, 0)
  }
}

case class XY(x: Int, y: Int) {
  def north: XY = copy(y = y - 1)
  def south: XY = copy(y = y + 1)
  def east: XY = copy(x = x + 1)
  def west: XY = copy(x = x - 1)

  def neighbors: Iterable[XY] = Iterable(north, west, east, south)
}

object XY {
  def readingOrder(xy1: XY, xy2: XY): Boolean = {
    (xy1.y < xy2.y) || (xy1.y == xy2.y && xy1.x < xy2.x)
  }
}

case class Cavern(walls: Set[XY]) {
  def isOpen(xy: XY): Boolean = !walls.contains(xy)
}

sealed trait BattleUnit {
  val hitPoints: Int
  val attackPower: Int = 3
  def isEnemy(other: BattleUnit): Boolean
  def damaged(hp: Int): BattleUnit
}

case class Goblin(hitPoints: Int = 200) extends BattleUnit {
  override def isEnemy(other: BattleUnit): Boolean = other match {
    case Elf(_, _) => true
    case _ => false
  }

  override def damaged(hp: Int): BattleUnit = copy(hitPoints = hitPoints - hp)
}

case class Elf(hitPoints: Int = 200, override val attackPower: Int = 3) extends BattleUnit {
  override def isEnemy(other: BattleUnit): Boolean = other match {
    case Goblin(_) => true
    case _ => false
  }

  override def damaged(hp: Int): BattleUnit = copy(hitPoints = hitPoints - hp)
}

sealed trait MoveResult

case class AdjacentToEnemy(position: XY, attacker: BattleUnit, enemies: NonEmptyList[XY]) extends MoveResult
case class Moved(nextXY: XY, battleUnit: BattleUnit) extends MoveResult
case object NoPossibleMoves extends MoveResult

sealed trait BattleResult

case object NoBattle extends BattleResult
case class Damage(xy: XY, battleUnit: BattleUnit) extends BattleResult
case class Kill(xy: XY) extends BattleResult

object CombatComplete extends Throwable

case class BattleBoard(cavern: Cavern, battleUnits: Map[XY, BattleUnit], awaitingTurn: NonEmptyList[XY]) {
  def isValid: Boolean = awaitingTurn.forall(battleUnits.contains) && battleUnits.keys.forall(cavern.isOpen)
  def isOngoing: Boolean = battleUnits.values.foldLeft(0){
    case (acc, _: Elf) => acc | 1
    case (acc, _: Goblin) => acc | 2
  } == 3
  def isOpen(xy: XY): Boolean = !cavern.walls.contains(xy) && !battleUnits.contains(xy)


  def move(): MoveResult = {
    val currentXY = awaitingTurn.head
    val currentUnit = battleUnits(currentXY)

    // Returns the shortest path to one of the targets, if one exists
    // If multiple targets are connected via a shortest path, return the least (w/r/t XY.readingOrder)
    // If multiple shortest paths exist to the same target, return one whose first step is least (w/r/t XY.readingOrder)
    def bfs(targets: Set[XY]): Option[NonEmptyList[XY]] = {
      val queue = MQueue.empty[NonEmptyList[XY]]
      // if (xy, k) ∈ visited, this means it was visited by a partial path of length k
      // if (xy, k1) visits where k1 > k, this path has no chance of being shortest
      val visited = TrieMap.empty[XY, Int]
      targets.foreach{ target =>
        visited.put(target, 1)
        queue.enqueue(nonEmptyList(target))
      }

      @tailrec
      def iter(bestCurrentPath: Option[NonEmptyList[XY]]): Option[NonEmptyList[XY]] = {
        def enqueueNeighbors(path: NonEmptyList[XY]): Unit = for {
          nbr <- path.head.neighbors if isOpen(nbr) && !path.contains(nbr)
        } {
          visited.get(nbr) match {
            case Some(length) if length < path.length + 1 =>
            //
            case _ =>
              visited.put(nbr, path.length + 1)
              queue.enqueue(asNonEmptyList(nbr :: path))
          }
        }
        (queue.dequeueFirst(_ => true), bestCurrentPath) match {
          case (None, _) => bestCurrentPath
          case (Some(path), Some(bestPath)) if bestPath.length < path.length =>
            iter(bestCurrentPath)
          case (Some(path), None) if path.head.neighbors.toSet.contains(currentXY) =>
            iter(Some(path))
          case (Some(path), None) =>
            enqueueNeighbors(path)
            iter(bestCurrentPath)
          case (Some(path), Some(bestPath)) if path.head.neighbors.toSet.contains(currentXY) => // at this point we know that path.length <= bestPath.length
            if (path.length < bestPath.length || XY.readingOrder(path.head, bestPath.head)) {
              iter(Some(path))
            } else {
              iter(bestCurrentPath)
            }
          case (Some(path), Some(bestPath)) if path.length == bestPath.length =>
            iter(bestCurrentPath)
          case (Some(path), _) =>
            enqueueNeighbors(path)
            iter(bestCurrentPath)
        }
      }

      iter(None)
    }

    val enemies = battleUnits.collect {
      case (xy, other) if currentUnit.isEnemy(other) => xy
    }.toSet
    if (enemies.isEmpty) {
      throw CombatComplete
    }
    val enemyNeighbors = enemies.toList.intersect(currentXY.neighbors.toSeq)
    enemyNeighbors match {
      case Nil =>
        val openSquaresInRange = enemies.flatMap { enemy =>
          enemy.neighbors.filter(isOpen)
        }
        if (openSquaresInRange.isEmpty) {
          NoPossibleMoves
        } else {
          bfs(openSquaresInRange) match {
            case None => NoPossibleMoves
            case Some(path) =>
              val nextXY = path.head
              val enemyNeighborsAfterMove = enemies.toList.intersect(nextXY.neighbors.toSeq)
              enemyNeighborsAfterMove match {
                case Nil => Moved(nextXY, currentUnit)
                case es: NonEmptyList[XY] => AdjacentToEnemy(nextXY, currentUnit, es)
              }
          }
        }
      case es: NonEmptyList[XY] => AdjacentToEnemy(currentXY, currentUnit, es)
    }
  }

  def doBattle(moveResult: MoveResult): BattleResult = moveResult match {
    case NoPossibleMoves | Moved(_, _) => NoBattle
    case AdjacentToEnemy(_, attacker, enemies) =>
      val (targetXY, targetUnit) = enemies.map { pos =>
        pos -> battleUnits(pos)
      }.minBy { case (pos, enemy) =>
        (enemy.hitPoints, pos.y, pos.x)
      }
      val damagedTarget = targetUnit.damaged(attacker.attackPower)
      if (damagedTarget.hitPoints <= 0) {
        Kill(targetXY)
      } else {
        Damage(targetXY, damagedTarget)
      }
  }

  def finalize(moveResult: MoveResult, battleResult: BattleResult): BattleBoard = (moveResult, battleResult) match {
    case (NoPossibleMoves, _) => BattleBoard.from(cavern, battleUnits, awaitingTurn.tail)
    case (Moved(nextXY, battleUnit), _) =>
      val h = awaitingTurn.head
      val nextBattleUnits = battleUnits - h + (nextXY -> battleUnit)
      BattleBoard.from(cavern, nextBattleUnits, awaitingTurn.tail)
    case (AdjacentToEnemy(nextXY, attacker, _), Damage(xy, damagedUnit)) =>
      val h = awaitingTurn.head
      val nextBattleUnits = battleUnits - h + (nextXY -> attacker) + (xy -> damagedUnit)
      BattleBoard.from(cavern, nextBattleUnits, awaitingTurn.tail)
    case (AdjacentToEnemy(nextXY, attacker, _), Kill(xy)) =>
      val h = awaitingTurn.head
      val nextBattleUnits = battleUnits - h - xy + (nextXY -> attacker)
      val nextAwaitingTurn = awaitingTurn.tail.filterNot(_ == xy)
      BattleBoard.from(cavern, nextBattleUnits, nextAwaitingTurn)
    case (AdjacentToEnemy(_,_,_), NoBattle) =>
      throw new IllegalStateException()
  }

  def takeTurn: BattleBoard = {
    val moveResult = move()
    val battleResult = doBattle(moveResult)
    finalize(moveResult, battleResult)
  }
}

object BattleBoard extends LazyLogging {

  def from(cavern: Cavern, battleUnits: Map[XY, BattleUnit], awaitingTurn: List[XY] = Nil): BattleBoard = {
    if (battleUnits.keys.exists(cavern.walls.contains)) {
      throw new IllegalArgumentException
    } else awaitingTurn match {
      case Nil => battleUnits.keys.toList.sortWith(XY.readingOrder) match {
        case Nil => throw new IllegalArgumentException
        case awaitingTurn: NonEmptyList[XY] => BattleBoard(cavern, battleUnits, awaitingTurn)
      }
      case xys: NonEmptyList[XY] if xys.forall(battleUnits.contains) =>
        BattleBoard(cavern, battleUnits, xys)
      case _ =>
        throw new IllegalArgumentException
    }
  }

  def parse(s: String): BattleBoard = {
    val walls = MHashSet.empty[XY]
    val battleUnits = TrieMap.empty[XY, BattleUnit]

    def parseLine(line: String, y: Int): Unit = {
      line.zipWithIndex.foreach {
        case ('#', x) => walls.add(XY(x,y))
        case ('.', _) => // Open space
        case ('E', x) => battleUnits.put(XY(x,y), Elf())
        case ('G', x) => battleUnits.put(XY(x,y), Goblin())
        case (c, _) if c.isWhitespace => //
        case (other, x) =>
          logger.warn(s"Unexpected character $other encountered at coordinates ($x, $y).")
      }
    }

    s.split("\n").zipWithIndex.foreach{ case (line, y) => parseLine(line, y) }

    BattleBoard.from(Cavern(walls.toSet), battleUnits.toMap)
  }
}