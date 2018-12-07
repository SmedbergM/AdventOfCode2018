package day4

import java.time.{Duration, LocalDateTime}
import scala.collection.concurrent.TrieMap
import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import common.AdventApp
import scalaj.http.Http

object Day4Part1 extends AdventApp {
  val guardLogEntries = Day4.fetchChallenge(sessionId)
  val shifts = Day4.getShifts(guardLogEntries)

  val challengeResponse = Day4.task1(shifts)
  println(s"Task 1: ${challengeResponse}")
}

object Day4Part2 extends AdventApp {
  val guardLogEntries = Day4.fetchChallenge(sessionId)
  val shifts = Day4.getShifts(guardLogEntries)

  val challengeResponse = Day4.task2(shifts)
  println(s"Task 2: ${challengeResponse}")
}

object Day4 extends LazyLogging {
  def fetchChallenge(sessionId: String): List[GuardLogEntry] = {
    val url = "https://adventofcode.com/2018/day/4/input"
    val request = Http(url)
      .cookie("session", sessionId)
    request.asString.body.split("\n").flatMap(GuardLogEntry.parse).toList
  }

  def task1(shifts: List[Shift]): Option[Long] = {
    shifts.headOption.map { _ =>
      val sleepTimeByGuard = TrieMap.empty[Int, Long]
      shifts.foreach { shift =>
        sleepTimeByGuard.getOrDefaultAndUpdate(shift.shiftStart.guardId, 0, _ + shift.sleepTime)
      }
      val (guardId,_) = sleepTimeByGuard.maxBy[Long](p => p._2)

      val asleepByMinute = TrieMap.empty[Int, Int]
      for {
        shift <- shifts
          if shift.shiftStart.guardId == guardId
        nap <- shift.naps
        i <- nap.fallAsleep.timestamp.getMinute until nap.wakeUp.timestamp.getMinute
      } asleepByMinute.getOrDefaultAndUpdate(i, 0, _ + 1)
      val sleepiestMinute = asleepByMinute.maxBy(_._2)._1
      guardId * sleepiestMinute
    }
  }

  def task2(shifts: List[Shift]): Option[Int] = {
    shifts.headOption.map { _ =>
      val sleepTimeByGuardAndMinute = TrieMap.empty[(Int, Int), Long]
      for {
        shift <- shifts
        nap <- shift.naps
        i <- nap.fallAsleep.timestamp.getMinute until nap.wakeUp.timestamp.getMinute
      } sleepTimeByGuardAndMinute.getOrDefaultAndUpdate(shift.shiftStart.guardId -> i, 0, _ + 1)
      val ((guardId, idx), _) = sleepTimeByGuardAndMinute.maxBy(_._2)
      guardId * idx
    }
  }

  def getShifts(logEntries: List[GuardLogEntry]): List[Shift] = {
    val sortedEntries = logEntries.sortWith((p0,p1) => p0.timestamp.isBefore(p1.timestamp))
    def getShiftsIter(optPartialShift: Option[Shift], entries: List[GuardLogEntry], acc: List[Shift]): List[Shift] = {
      entries match {
        case Nil => optPartialShift ++: acc
        case (ss : ShiftStart) :: rest =>
          getShiftsIter(Some(Shift(ss, Set.empty)), rest, optPartialShift ++: acc)
        case (fa : FallAsleep) :: (wu : WakeUp) :: rest =>
          val nap = Nap(fa, wu)
          val nextPartialShift = optPartialShift.map { shift =>
            shift.copy(naps = shift.naps + nap)
          }.orElse {
            logger.warn("Nap found but no shift to add it to!")
            None
          }
          getShiftsIter(nextPartialShift, rest, acc)
        case entry0 :: rest =>
          logger.warn(s"Invalid guard logs encountered: partial shift ${optPartialShift}, next log entry ${entry0}!")
          getShiftsIter(optPartialShift, rest, acc)
      }
    }
    getShiftsIter(None, sortedEntries, Nil)
  }

  private implicit class RichTrieMap[K,V](m: TrieMap[K,V]) {
    def getOrDefaultAndUpdate(k: K, v: => V, f: V => V): Unit = {
      m += (k -> f(m.getOrElse(k, v)))
    }
  }
}

sealed trait GuardLogEntry {
  val timestamp: LocalDateTime
}

case class ShiftStart(timestamp: LocalDateTime, guardId: Int) extends GuardLogEntry

case class FallAsleep(timestamp: LocalDateTime) extends GuardLogEntry

case class WakeUp(timestamp: LocalDateTime) extends GuardLogEntry

object GuardLogEntry extends LazyLogging {

  private def parseInt(s: String): Option[Int] = Try(Integer.parseInt(s)).toOption
  private def parseDateTime(yyyy: String, mm: String, dd: String, hh: String, min: String): Option[LocalDateTime] = for {
    year <- parseInt(yyyy)
    month <- parseInt(mm)
    day <- parseInt(dd)
    hour <- parseInt(hh)
    minute <- parseInt(min)
  } yield LocalDateTime.of(year, month, day, hour, minute)

  def parse(s: String): Option[GuardLogEntry] = {
    val timestampPat = """\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\]\s*"""
    val shiftStartPat = (timestampPat + """Guard #(\d+) begins shift""").r
    val fallAsleepPat = (timestampPat + """falls asleep""").r
    val wakeUpPat = (timestampPat + """wakes up""").r

    shiftStartPat.unapplySeq(s).flatMap {
      case yyyy :: mm :: dd :: hh :: min :: guardId :: Nil => for {
        timestamp <- parseDateTime(yyyy, mm, dd, hh, min)
        guard <- parseInt(guardId)
      } yield ShiftStart(timestamp, guard)
      case other =>
        logger.warn(s"Unexpected shift start regex groups ${other}")
        None
    }.orElse { fallAsleepPat.unapplySeq(s).flatMap {
      case yyyy :: mm :: dd :: hh :: min :: Nil => for {
        timestamp <- parseDateTime(yyyy, mm, dd, hh, min)
      } yield FallAsleep(timestamp)
      case other =>
        logger.warn(s"Unexpected fall asleep regex groups ${other}")
        None
    }}.orElse { wakeUpPat.unapplySeq(s).flatMap {
      case yyyy :: mm :: dd :: hh :: min :: Nil => for {
        timestamp <- parseDateTime(yyyy, mm, dd, hh, min)
      } yield WakeUp(timestamp)
      case other =>
        logger.warn(s"Unexpected wakeup regex groups ${other}")
        None
    }}
  }
}

case class Nap(fallAsleep: FallAsleep, wakeUp: WakeUp) {
  assert(fallAsleep.timestamp.isBefore(wakeUp.timestamp))
}

case class Shift(shiftStart: ShiftStart, naps: Set[Nap]) {
  def sleepTime: Long = naps.foldLeft[Long](0){ case (acc, nap) =>
    acc + Duration.between(nap.fallAsleep.timestamp, nap.wakeUp.timestamp).toMinutes
  }
}