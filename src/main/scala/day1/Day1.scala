package day1

import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import scalaj.http.Http

object Day1Part1 extends App {
  val sessionId = sys.env("SESSION_ID")
  val challenge = Day1.fetchChallenge(sessionId)

  println(s"Part 1: ${Day1.task1(challenge)}")
}

object Day1Part2 extends App {
  val sessionId = sys.env("SESSION_ID")
  val challenge = Day1.fetchChallenge(sessionId)

  println(s"Part 2: ${Day1.task2(challenge)}")
}

object Day1 extends LazyLogging {
  private val challengeURL = "https://adventofcode.com/2018/day/1/input"
  def fetchChallenge(sessionId: String): Seq[Int] = {
    val request = Http(challengeURL)
      .cookie("session", sessionId)
    val response = request.asString
    parseChallenge(response.body)
  }

  def parseChallenge(challenge: String): Seq[Int] = {
    val pat = """([+-])(\d+)""".r
    challenge.split("\n").toVector.flatMap {
      case pat("-", digits) => Try(-Integer.parseInt(digits)).toOption
      case pat(_, digits) => Try(Integer.parseInt(digits)).toOption
      case _ => None
    }
  }

  def task1(challenge: Seq[Int]): Int = challenge.sum

  def task2(challenge: Seq[Int]): Int = {
    val repeatedChallenge = Stream.continually(0).flatMap(_ => challenge)
    def task2Iter(seen: Set[Int], sum: Int, xs: Stream[Int]): Int = {
      if (seen.contains(sum)) {
        sum
      } else xs match {
        case Stream.Empty => throw new IllegalArgumentException
        case head #:: rest =>
          logger.debug("Current sum {}; {} total sums seen", sum, seen.size)
          task2Iter(seen + sum, sum + head, rest)
      }
    }
    task2Iter(Set.empty, 0, repeatedChallenge)
  }
}