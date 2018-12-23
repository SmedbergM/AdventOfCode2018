package day9

import scala.annotation.tailrec
import scala.util.Try

import common.AdventApp
import scalaj.http.Http

object Day9Part1 extends AdventApp {
  val Some((nPlayers, lastMarble)) = Day9.fetchChallenge(sessionId)

  val score = Day9.playMarbleGame(nPlayers, lastMarble)
  val winner = score.maxBy(_._2)
  println(s"Task 1: Winner ${winner}")

}

object Day9 {
  def fetchChallenge(sessionId: String): Option[(Int, Int)] = {
    val url = "https://adventofcode.com/2018/day/9/input"
    val pat = """(\d+) players; last marble is worth (\d+) points""".r
    val request = Http(url)
      .cookie("session", sessionId)
    val response = request.asString
    for {
      n :: p :: Nil <- pat.unapplySeq(response.body.trim)
      nPlayers <- parseInt(n)
      points <- parseInt(p)
    } yield nPlayers -> points
  }

  def playMarbleGame(nPlayers: Int, lastMarble: Int): Map[Int, Int] = {
    @tailrec
    def playMarbleGameIter(mg: MarbleGame): Map[Int, Int] = mg match {
      case MarbleGame(_, score, _, nextMarble) if nextMarble > lastMarble => score
      case _ => playMarbleGameIter(mg.play)
    }
    playMarbleGameIter(MarbleGame.empty(nPlayers))
  }

  private def parseInt(s: String) = Try(Integer.parseInt(s)).toOption
}

case class MarbleGame(player: Int => Int, score: Map[Int, Int], marbles: Vector[Int], nextMarble: Int) {
  assert(marbles.nonEmpty)
  // The head of `marbles` is the currently active marble
  // "clockwise" is to the right

  def play: MarbleGame = {
    nextMarble % 23 match {
      case 0 =>
        val ris = marbles.reverseIterator.zip(marbles.reverseIterator.drop(1))
        val (n6, n7) = ris.drop(5).next()
        val marbleLoop = Stream.continually(0).flatMap(_ => marbles)
        val nextMarbles = marbleLoop.dropWhile(_ != n6).take(marbles.length - 1).toVector
        val p = player(nextMarble)
        val nextScore = score + (p -> (score.getOrElse(p, 0) + nextMarble + n7))
        MarbleGame(player, nextScore, nextMarbles, nextMarble + 1)
      case _ =>
        val marbleLoop = Stream.continually(0).flatMap(_ => marbles)
        val nextMarbles = nextMarble +: marbleLoop.drop(2).take(marbles.length).toVector
        MarbleGame(player, score, nextMarbles, nextMarble + 1)
    }
  }
}

object MarbleGame {
  def empty(nPlayers: Int): MarbleGame = {
    val score = (0 until nPlayers).map(p => p -> 0).toMap
    MarbleGame(k => 1 + ((k-1) % nPlayers), score, Vector(0), 1)
  }
}