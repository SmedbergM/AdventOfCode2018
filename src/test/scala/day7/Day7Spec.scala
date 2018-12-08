package day7

import org.scalatest.FlatSpec

class Day7Spec extends FlatSpec {


  val input = """Step C must be finished before step A can begin.
                |Step C must be finished before step F can begin.
                |Step A must be finished before step B can begin.
                |Step A must be finished before step D can begin.
                |Step B must be finished before step E can begin.
                |Step D must be finished before step E can begin.
                |Step F must be finished before step E can begin.""".stripMargin

  val challenge = input.split("\n").flatMap(Day7.parseEdge).toSet

  assertResult(7)(challenge.size)

  "task2" should "perform steps in parallel" in {
    def estimator(taskID: String): TaskStatus = {
      val taskDuration: Int = taskID.headOption.fold(0) {
        c => c - 'A'
      }
      TaskStatus(taskID, taskDuration)
    }

    assertResult(15)(Day7.task2(challenge, 2, estimator))
  }
}
