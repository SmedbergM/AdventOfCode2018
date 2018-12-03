package day2

import scala.collection.concurrent.TrieMap

import scalaj.http.Http

object Day2Part1 extends App {
  val sessionId = sys.env("SESSION_ID")
  val challenge = Day2.parseChallenge(Day2.fetchChallenge(sessionId))

  println(s"Part 1: ${Day2.task1(challenge)}")
}

object Day2Part2 extends App {
  val sessionId = sys.env("SESSION_ID")
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
    val seen = new Root

    def task2Iter(challengeIter: Seq[String]): Option[String] = {
      challengeIter.headOption match {
        case None => None
        case Some(s) =>
          seen.insertChildAndCheckNeighbor(s.toList) match {
            case None => task2Iter(challengeIter.drop(1))
            case Some(neighbor) => Some(neighbor.mkString)
          }
      }
    }

    task2Iter(challenge)
  }

  sealed trait SearchTree {
    protected val children: TrieMap[Char, Child] = TrieMap.empty

    def hasPath(p: List[Char]): Boolean = p match {
      case Nil => children.isEmpty
      case p0 :: rest => children.get(p0) match {
        case None => false
        case Some(child) => child.hasPath(rest)
      }
    }

    def insertChild(p: List[Char]): Unit = p match {
      case Nil => ()
      case p0 :: rest =>
        val child = children.getOrElseUpdate(p0, new Child(p0))
        child.insertChild(rest)
    }

    def insertChildren(ps: Iterable[List[Char]]): Unit = {
      ps.foreach(insertChild)
    }

    def insertChildAndCheckNeighbor(p: List[Char]): Option[List[Char]] = p match {
      case Nil => None
      case p0 :: rest =>
        val optNeighbor = children.collectFirst {
          case (c, other) if c != p0 && other.hasPath(rest) => c :: rest
        }
        optNeighbor match {
          case Some(neighbor) =>
            insertChild(p)
            Some(neighbor)
          case None =>
            children.getOrElseUpdate(p0, new Child(p0)).insertChildAndCheckNeighbor(rest).map { cs =>
              p0 :: cs
            }
        }
    }
  }

  class Root extends SearchTree {

    override def insertChildAndCheckNeighbor(p: List[Char]): Option[List[Char]] = p match {
      case Nil => None
      case p0 :: rest =>

        for {
          neighbor <- children.getOrElseUpdate(p0, new Child(p0)).insertChildAndCheckNeighbor(rest)
            if neighbor.size == p.size
          filteredNeighbor = neighbor.zip(p).filter(t => t._1 == t._2)
            if filteredNeighbor.size == p.size - 1
        } yield filteredNeighbor.map(_._1)
        super.insertChildAndCheckNeighbor(p).flatMap {
          case neighbor if neighbor.size != p.size => None
          case neighbor =>
            val filteredNeighbor = neighbor.zip(p).collect {
              case (nc, pc) if nc == pc => nc
            }
            Option(filteredNeighbor).filter(_.size == p.size - 1)
        }
    }
  }

  class Child(value: Char) extends SearchTree
}
