package day2

import scala.collection.JavaConverters._

import common.AdventApp
import scalaj.http.Http

object Day2Part1 extends AdventApp {
  val challenge = Day2.parseChallenge(Day2.fetchChallenge(sessionId))

  println(s"Part 1: ${Day2.task1(challenge)}")
}

object Day2Part2 extends AdventApp {
  val challenge = Day2.parseChallenge(Day2.fetchChallenge(sessionId))

  println(s"Part 2: ${Day2.task2(challenge)}")
}

object Day2 {
  def fetchChallenge(sessionId: String): String = {
    val url = "https://adventofcode.com/2018/day/2/input"
    val request = Http(url)
      .cookie("session", sessionId)
    request.asString.body
  }

  def parseChallenge(challenge: String): Seq[String] = {
    challenge.split("\n").filter(_.nonEmpty).toVector
  }

  def charCounts(s: String): Map[Char, Int] = {
    s.par.groupBy(c => c).mapValues(_.size).seq.toMap
  }

  def task1(challenge: Seq[String]): Int = {
    val (count2, count3) = challenge.foldLeft((0,0)){ case ((c2, c3), s) =>
      val (has2, has3) = charCounts(s).foldLeft(Option.empty[Char], Option.empty[Char]){
        case ((None, h3), (c,2)) => Some(c) -> h3
        case ((h2, None), (c,3)) => h2 -> Some(c)
        case (acc, _) => acc
      }
      (c2 + has2.size) -> (c3 + has3.size)
    }
    count2 * count3
  }

  def task2(challenge: Seq[String]): Option[String] = {
    def task2Iter(challengeIter: Seq[String], searchTree: SearchTree): Option[String] = challengeIter.headOption match {
      case None => None
      case Some(s) =>
        searchTree.insertPathAndCheckNeighbor(s.codePoints().iterator().asScala.toList.map(_.intValue())) match {
          case (_, Some(path)) =>
            Option(new String(path.toArray, 0, path.size))
          case (nextSearchTree, _) => task2Iter(challengeIter.drop(1), nextSearchTree)
        }
    }

    task2Iter(challenge, SearchTree.empty)
  }

  case class SearchTree(isTerminal: Boolean, children: Map[Int, SearchTree]) {
    def hasPath(p: List[Int]): Boolean = p match {
      case Nil => isTerminal
      case p0 :: rest => children.get(p0).fold(false)(_.hasPath(rest))
    }

    def insertPath(p: List[Int]): SearchTree = p match {
      case Nil =>
        this.copy(isTerminal = true)
      case p0 :: rest =>
        val nextChildren = children + (p0 -> children.getOrElse(p0, SearchTree.empty).insertPath(rest))
        this.copy(children = nextChildren)
    }

    def insertPathAndCheckNeighbor(p: List[Int]): (SearchTree, Option[List[Int]]) = p match {
      case Nil => insertPath(p) -> None
      case p0 :: rest =>
        val optNeighbor = children.collectFirst {
          case (c, child) if c != p0 && child.hasPath(rest) => rest
        }
        optNeighbor match {
          case Some(neighbor) =>
            insertPath(p) -> Some(neighbor)
          case None =>
            val (nextChild, optNeighbor) = children.getOrElse(p0, SearchTree.empty).insertPathAndCheckNeighbor(rest)
            this.copy(children = children + (p0 -> nextChild)) -> optNeighbor.map(p0 :: _)
        }
    }
  }

  object SearchTree {
    def empty: SearchTree = SearchTree(false, Map.empty)
  }
}
