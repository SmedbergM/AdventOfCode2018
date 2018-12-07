package day5

import common.AdventApp
import scalaj.http.Http

object Day5Part1 extends AdventApp {
  val challenge = Day5.fetchChallenge(sessionId)

  println(s"Task 1: After cancellation, ${Day5.simplify(challenge).size} characters remain.")
}

object Day5Part2 extends AdventApp {
  val challenge = Day5.fetchChallenge(sessionId)

  val (m, len) = Day5.task2(challenge)
  println(s"Task 2: By eliminating ${m} we get length ${len}")
}

object Day5 {
  def fetchChallenge(sessionId: String): String = {
    val url = "https://adventofcode.com/2018/day/5/input"
    val request = Http(url)
      .cookie("session", sessionId)
    request.asString.body.trim
  }

  def simplify(input: String): String = {
    input.foldRight(List.empty[Char]){
      case (c, head :: rest)
        if c.isLower && head == c.toUpper || head.isLower && c == head.toUpper =>
        rest
      case (c, acc) => c :: acc
    }.mkString
  }

  def task2(input: String): (String, Int) = {
    val monomers = input.toSet[Char].map(c => c.toLower -> c.toUpper)
    val simplified = monomers.map { case (low, up) =>
      (low, up) -> simplify(input.replace(s"${low}", "").replace(s"${up}", "")).length
    }
    val ((lower, upper), len) = simplified.minBy(_._2)
    s"${lower}/${upper}" -> len
  }
}
