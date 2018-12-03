package day3

import scala.collection.concurrent.TrieMap
import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import scalaj.http.Http

object Day3Part1 extends App {
  val sessionId = sys.env("SESSION_ID")
  val claims = Day3.fetchChallenge(sessionId)

  println(s"I got ${Day3.task1(claims)} disputed squares")
}

object Day3Part2 extends App {
  val sessionId = sys.env("SESSION_ID")
  val claims = Day3.fetchChallenge(sessionId)

  println(s"I found ${Day3.task2(claims)} non-overlapping any other claims.")
}

object Day3 extends LazyLogging {

  def fetchChallenge(sessionId: String): List[Claim] = {
    val url = "https://adventofcode.com/2018/day/3/input"
    val request = Http(url)
      .cookie("session", sessionId)
    request.asString.body.split("\n").flatMap(parseClaim).toList
  }

  def parseClaim(s: String): Option[Claim] = {
    val pat = """#(\d+)\s*@\s*(\d+),(\d+):\s*(\d+)x(\d+)""".r
    pat.unapplySeq(s).flatMap {
      case g0 :: g1 :: g2 :: g3 :: g4 :: Nil => for {
        id <- Try(Integer.parseInt(g0)).toOption
        xOffset <- Try(Integer.parseInt(g1)).toOption
        yOffset <- Try(Integer.parseInt(g2)).toOption
        width <- Try(Integer.parseInt(g3)).toOption
        height <- Try(Integer.parseInt(g4)).toOption
      } yield Claim(id, xOffset, yOffset, width, height)

      case _ => None
    }
  }

  def task1(claims: List[Claim]): Int = {
    sealed trait Coverage {
      def increment: Coverage
    }
    case object Unused extends Coverage {
      override def increment: Coverage = Claimed
    }
    case object Claimed extends Coverage {
      override def increment: Coverage = Disputed
    }
    case object Disputed extends Coverage {
      override def increment: Coverage = Disputed
    }
    val sortedClaims = claims.sortBy(c => (c.diagonal, c.xOffset))

    val coverages = TrieMap.empty[(Int, Int), Coverage]
    val (disputedCount, _) = sortedClaims.foldLeft((0,-1)){ case ((disputeCount, diagonal), claim) =>
      val nextDiagonal = claim.diagonal
      val nextDisputeCount = if (nextDiagonal > diagonal) {
        // then we can finalize those coverages closer to the origin
        val nextDisputeCount = coverages.foldLeft(disputeCount) {
          case (a, (k, coverage)) if Claim.diagonal(k) < nextDiagonal =>
            coverages.remove(k)
            coverage match {
              case Disputed => a + 1
              case _ => a
            }
          case (a, _) => a
        }
        coverages.retain { case (k, _) => Claim.diagonal(k) >= nextDiagonal}
        logger.debug(s"Moving to diagonal ${nextDiagonal}; ${coverages.size} squares in memory")
        nextDisputeCount
      } else disputeCount
      (claim.xOffset until claim.xOffset + claim.width).foreach { x =>
        (claim.yOffset until claim.yOffset + claim.height).foreach { y =>
          val key = (x,y)
          coverages += (key -> coverages.getOrElse(key, Unused).increment)
        }
      }
      (nextDisputeCount, nextDiagonal)
    }
    // After the fold, the last diagonal hasn't been removed from `coverages`
    disputedCount + coverages.values.count(_ == Disputed)
  }

  def task2(claims: List[Claim]): Option[Int] = {
    def nonOverLapping(claim: Claim): Boolean = {
      def nonOverLappingIter(candidates: List[Claim]): Boolean = candidates match {
        case Nil => true
        case head :: rest if head == claim => nonOverLappingIter(rest)
        case head :: _ if claim.overlaps(head) => false
        case _ :: rest => nonOverLappingIter(rest)
      }
      nonOverLappingIter(claims)
    }
    claims.find(nonOverLapping).map(_.id)
  }
}

case class Claim(id: Int, xOffset: Int, yOffset: Int, width: Int, height: Int) {
  def diagonal: Int = Claim.diagonal(xOffset, yOffset)

  def overlaps(other: Claim): Boolean = {
    def rangeOverlaps(range0: (Int, Int), range1: (Int, Int)): Boolean = {
      range0._1 <= range1._1 && range1._1 < range0._2
    } || {
      range1._1 <= range0._1 && range0._1 < range1._2
    }
    def xOverlaps: Boolean = rangeOverlaps((this.xOffset, this.xOffset + this.width), (other.xOffset, other.xOffset + other.width))
    def yOverlaps: Boolean = rangeOverlaps((this.yOffset, this.yOffset + this.height), (other.yOffset, other.yOffset + other.height))
    xOverlaps && yOverlaps
  }
}

object Claim {
  def diagonal(k: (Int, Int)): Int = k._1 + k._2
}

