package day12

import scala.annotation.tailrec

import com.typesafe.scalalogging.LazyLogging
import common.AdventApp
import scalaj.http.Http

object Day12Part1 extends AdventApp with LazyLogging {
  val (optState, rewriteRules) = Day12.fetchChallenge(sessionId)

  val evolved = Day12.evolveState(optState.get, rewriteRules, 20)
  logger.info(s"Evolved state checksum: ${evolved.checksum}")

  val t = BigInt(50) * 1000 * 1000 * 1000
  val longEvolved = optState match {
    case None => throw new IllegalArgumentException
    case Some(state) =>
      Day12.evolveStateLong(state, rewriteRules, t)
  }
  logger.info(s"Long evolved state checksum: ${longEvolved.checksum}")
}

object Day12 extends LazyLogging {
  def fetchChallenge(sessionId: String): (Option[State], RewriteRules) = {
    val url = "https://adventofcode.com/2018/day/12/input"
    val request = Http(url)
      .cookie("session", sessionId)
    val response = request.asString
    parseChallenge(response.body)
  }

  def parseChallenge(challenge: String): (Option[State], RewriteRules) = {
    val statePat = """initial state:\s*([.#]+)""".r
    val rewritePat = """([.#]{5})\s*=>\s*([.#])""".r
    challenge.split("\n").foldLeft(Option.empty[State] -> RewriteRules.Empty){
      case ((None, rr), statePat(state)) => Some(State(state, 0)) -> rr
      case ((Some(_), _), statePat(_)) =>
        throw new IllegalArgumentException("Two state lines encountered!")
      case ((optState, RewriteRules(rules)), rewritePat(pattern, result)) =>
        optState -> RewriteRules(rules + (pattern -> result.head))
      case (acc, _) =>
        acc
    }
  }

  @tailrec
  def evolveState(state: State, rewriteRules: RewriteRules, times: BigInt): State = if (times <= 0) {
    state
  } else {
    evolveState(state.evolve(rewriteRules), rewriteRules, times - 1)
  }

  def evolveStateLong(state: State, rewriteRules: RewriteRules, totalGenerations: BigInt): State = {
    @tailrec
    def evolveIter(currentState: State, currentGeneration: BigInt, seen: Map[String, (State, BigInt)]): State = {
      if (currentGeneration == totalGenerations) {
        currentState
      } else {
        seen.get(currentState.flowerPots) match {
          case None =>
            val nextSeen = seen + (currentState.flowerPots -> (currentState -> currentGeneration))
            evolveIter(currentState.evolve(rewriteRules), currentGeneration + 1, nextSeen)
          case Some((previousState, previousGeneration)) =>
            val pd = currentGeneration - previousGeneration
            logger.info(s"Found a repeat; period length $pd; previous state ${previousState}")
            val indexShift = currentState.firstIndex - previousState.firstIndex
            val (q, rem) = (totalGenerations - currentGeneration) /% pd
            val nextState = currentState.copy(firstIndex = currentState.firstIndex + q * indexShift)
            (0L until rem.longValue()).foldLeft(nextState){ case (acc, _) => acc.evolve(rewriteRules) }
        }
      }
    }

    evolveIter(state.evolve(rewriteRules), 1, Map(state.flowerPots -> (state, 0)))
  }

  private def streamFromBig(x: BigInt): Stream[BigInt] = x #:: streamFromBig(x+1)

  case class State(flowerPots: String, firstIndex: BigInt) {
    assert(flowerPots.forall(_.isValid))

    def checksum: BigInt = flowerPots.zip(streamFromBig(firstIndex)).foldLeft(BigInt(0)){
      case (acc, ('#', idx)) => acc + idx
      case (acc, _) => acc
    }

    def evolve(rewriteRules: RewriteRules): State = {
      val stripRightDotsPat = """(.*?)[.]*""".r
      val indices: Stream[BigInt] = streamFromBig(firstIndex - 2)
      val paddedFlowerPots = s"....${flowerPots}...."
      val indexedNbhds = paddedFlowerPots.sliding(5).toStream.zip(indices)
      indexedNbhds.foldLeft(Option.empty[(BigInt, String)]) {
        case (None, (nbhd, idx)) =>
          rewriteRules.rules.get(nbhd) match {
            case Some('#') => Some(idx -> "#")
            case _ => None
          }
        case (Some((nextFirstIdx, pots)), (nbhd, _)) =>
          Some(nextFirstIdx -> (pots + rewriteRules.rules.getOrElse(nbhd,'.')))
      } match {
        case None => State("", 0)
        case Some((nextFirstIndex, stripRightDotsPat(nextPots))) =>
          State(nextPots, nextFirstIndex)
      }
    }
  }

  case class RewriteRules(rules: Map[String, Char]) {
    for {
      (pattern, result) <- rules
    } {
      assert(pattern.length == 5)
      assert(pattern.forall(_.isValid))
      assert(result.isValid)
    }
  }

  object RewriteRules {
    val Empty: RewriteRules = RewriteRules(Map.empty)
  }


  private implicit class RichChar(c: Char) {
    def isValid: Boolean = c == '#' || c == '.'
  }
}
