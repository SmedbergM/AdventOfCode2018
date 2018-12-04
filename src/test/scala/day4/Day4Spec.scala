package day4

import java.time.LocalDateTime

import org.scalatest.FlatSpec

class Day4Spec extends FlatSpec {
  "parse" should "parse logs" in {
    val input0 = "[1518-03-26 23:50] Guard #173 begins shift"
    val input1 = "[1518-03-14 00:34] falls asleep"
    val input2 = "[1518-04-09 00:20] wakes up"

    val timestamp0 = LocalDateTime.of(1518, 3, 26, 23, 50)
    val timestamp1 = LocalDateTime.of(1518, 3, 14, 0, 34)
    val timestamp2 = LocalDateTime.of(1518, 4, 9, 0, 20)
    assertResult(Some(ShiftStart(timestamp0, 173)))(GuardLogEntry.parse(input0))
    assertResult(Some(FallAsleep(timestamp1)))(GuardLogEntry.parse(input1))
    assertResult(Some(WakeUp(timestamp2)))(GuardLogEntry.parse(input2))
  }
}
