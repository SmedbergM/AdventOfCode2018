package day14

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

import com.typesafe.scalalogging.LazyLogging

object Day14Part1 extends App with LazyLogging {
  val challenge = Day14.challenge

  logger.info(s"Next 10 after ${challenge}: ${Day14.task1(challenge)}")
}

object Day14Part2 extends App with LazyLogging {
  val challenge = Day14.challenge

  logger.info(s"Length until $challenge found: ${Day14.task2(challenge.toString)}")
}

object Day14 extends LazyLogging {
  def challenge: Int = Integer.parseInt(sys.env("DAY_14_CHALLENGE"))

  def task1(n: Int): String = {
    @tailrec
    def task1iter(board: RecipeBoard): String = if (board.recipes.length > n + 10) {
      board.recipes.slice(n, n + 10)
    } else {
      task1iter(board.next)
    }

    task1iter(RecipeBoard.initial)
  }

  def task2(s: String): Int = {
    val cs = "37".to[ArrayBuffer]
    @tailrec
    def iter(pos0: Int, pos1: Int): Int = {
      val score0 = cs(pos0).asDigit
      val score1 = cs(pos1).asDigit
      val nextSuffix = (score0 + score1).toString.toList
      val optResult = nextSuffix.foldLeft(Option.empty[Int]){
        case (Some(r), _) => Some(r)
        case (None, c) =>
          cs.append(c)
          if (cs.endsWith(s)) {
            Some(cs.length - s.length)
          } else {
            None
          }
      }
      optResult match {
        case Some(r) => r
        case None =>
          val nextPos0 = (pos0 + score0 + 1) % cs.length
          val nextPos1 = (pos1 + score1 + 1) % cs.length
          iter(nextPos0, nextPos1)
      }
    }

    iter(0, 1)
  }

  // all characters in `recipes` must be digits
  case class RecipeBoard(recipes: String, position0: Int, position1: Int) {
    def next: RecipeBoard = {
      val score0 = recipes(position0).asDigit
      val score1 = recipes(position1).asDigit
      val nextRecipes = recipes + (score0 + score1)
      val nextPos0 = (position0 + score0 + 1) % nextRecipes.length
      val nextPos1 = (position1 + score1 + 1) % nextRecipes.length
      RecipeBoard(nextRecipes, nextPos0, nextPos1)
    }
  }

  object RecipeBoard {
    val initial: RecipeBoard = RecipeBoard("37", 0, 1)
  }
}
