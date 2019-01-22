package day23

import scala.io.Source
import scala.util.Try
import scala.collection.immutable.Seq

import com.typesafe.scalalogging.LazyLogging
import scalaj.http.Http

object Day23Part1 extends App with LazyLogging {
  val nanobots = Day23.localNanobots.toSet

  val strongestBot = nanobots.maxBy(_.signalRadius)

  logger.info(s"Strongest nanobot $strongestBot can contact ${ nanobots.count(strongestBot.canContact)} bots")
}

object Day23 {
  def fetchRawChallenge(sessionId: String): String = {
    val url = "https://adventofcode.com/2018/day/23/input"
    val request = Http(url).cookie("session", sessionId)
    request.asString.body
  }

  def localRawChallenge: String = {
    Source.fromInputStream(getClass.getResourceAsStream("/23.challenge")).mkString
  }

  def localNanobots: Iterable[Nanobot] = {
    localRawChallenge.split("\n").flatMap(Nanobot.parse).to[Seq]
  }
}

case class Nanobot(x: Long, y: Long, z: Long, signalRadius: Long) {
  def canContact(other: Nanobot): Boolean = {
    val d = math.abs(x - other.x) + math.abs(y - other.y) + math.abs(z - other.z)
    d <= signalRadius
  }
}

object Nanobot {
  private val Pat = """pos=<(-?\d+),(-?\d+),(-?\d+)>,\s+r=(\d+)""".r
  def parse(s: String): Option[Nanobot] = s match {
    case Pat(px, py, pz, pr) => for {
      x <- Try(px.toLong).toOption
      y <- Try(py.toLong).toOption
      z <- Try(pz.toLong).toOption
      r <- Try(pr.toLong).toOption
    } yield Nanobot(x,y,z,r)
    case _ =>
      None
  }
}