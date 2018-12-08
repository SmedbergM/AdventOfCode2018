package day7

import common.AdventApp
import scalaj.http.Http

object Day7Part1 extends AdventApp {
  val challenge = Day7.fetchChallenge(sessionId)

  println(s"Task 1: ${Day7.task1(challenge)}")
}

object Day7Part2 extends AdventApp {
  val challenge = Day7.fetchChallenge(sessionId)

  println(s"Task 2: ${Day7.task2(challenge, 5)}")
}

object Day7 {
  def fetchChallenge(sessionId: String): Set[Edge] = {
    val url = "https://adventofcode.com/2018/day/7/input"
    val request = Http(url)
      .cookie("session", sessionId)
    request.asString.body.split("\n").flatMap(parseEdge).toSet
  }

  def parseEdge(line: String): Option[Edge] = {
    val pat = """Step (\w+) must be finished before step (\w+) can begin.""".r
    line match {
      case pat(f1, f2) => Some(Edge(f1, f2))
      case _ => None
    }
  }

  def task1(edges: Set[Edge]): String = {
    val vertices = edges.flatMap(_.toSet)
    val predecessors = edges.groupBy(_.post).map { case (k, es) => k -> es.map(_.pre) }
    def findSources(v: String, marked: List[String]): Set[String] = {
      predecessors.get(v) match {
        case None => Set(v).filterNot(marked.contains)
        case Some(preds) =>
          val srcs = preds.flatMap(findSources(_, marked))
          if (srcs.nonEmpty) {
            srcs
          } else Set(v).filterNot(marked.contains)
      }
    }

    def task1Iter(sorted: List[String], remaining: Set[String]): List[String] = {
      // invariant: every vertex occurs exactly once in sorted and remaining
      val sources = remaining.flatMap(findSources(_, sorted))
      if (sources.isEmpty) {
        if (remaining.nonEmpty) {
          throw new Exception("findSources is badly programmed")
        }
        sorted
      } else {
        val v = sources.min
        task1Iter(v :: sorted, remaining - v)
      }
    }

    task1Iter(Nil, vertices).reverse.mkString
  }

  def task2(edges: Set[Edge], numberOfWorkers: Int, estimator: String => TaskStatus = TaskStatus.of): Int = {
    val vertices = edges.flatMap( _.toSet )
    val sinks = vertices -- edges.map(_.pre)
    val predecessors = edges.groupBy(_.post).map{ case (k, es) => k -> es.map(_.pre)}

    // finds at most k sources in the induced subgraph not including `completed`
    def findSources(k: Int, completed: Set[String], inProgress: Iterable[String]): Set[String] = {
      if (k <= 0) {
        Set.empty
      } else {
        def findSourcesIter(xs: Set[String]): Set[String] = {
          xs.flatMap { x => predecessors.get(x) match {
            case None => Set(x).diff(completed)
            case Some(preds) =>
              val sources = findSourcesIter(preds).diff(completed)
              if (sources.isEmpty) {
                Set(x).diff(completed)
              } else sources
          }}
        }
        def kLeast(xs: Set[String]): Set[String] = {
          def kLeastIter(k1: Int, least: Set[String], rest: Set[String]): Set[String] = if (k1 <= 0) {
            least
          } else if (rest.size <= k1) {
            least ++ rest
          } else {
            val v = rest.min
            kLeastIter(k1 - 1, least + v, rest - v)
          }

          kLeastIter(k, Set.empty, xs)
        }
        val sources = findSourcesIter(sinks) -- inProgress
        kLeast(sources)
      }
    }

    def task2Iter(idx: Int, completedTaskIds: Set[String], tasksInProgress: List[TaskStatus]): Int = {
      val (nextCompleted, continued) = tasksInProgress.foldLeft(completedTaskIds -> List.empty[TaskStatus]){
        case ((acc0, acc1), TaskStatus(taskID, 0)) => (acc0 + taskID, acc1)
        case ((acc0, acc1), taskStatus) => (acc0, taskStatus.copy(timeRemaining = taskStatus.timeRemaining - 1) :: acc1)
      }
      val tasksToStart = findSources(numberOfWorkers - continued.size, nextCompleted, continued.map(_.taskID)).map(estimator)
      if (continued.isEmpty && tasksToStart.isEmpty) {
        idx
      } else {
        task2Iter(idx + 1, nextCompleted, continued ++ tasksToStart)
      }
    }

    task2Iter(0, Set.empty, Nil)
  }

}

case class TaskStatus(taskID: String, timeRemaining: Int)

object TaskStatus {
  def of(taskID: String): TaskStatus = TaskStatus(taskID, taskTime(taskID))

  def taskTime(taskID: String): Int = taskID.foldLeft(0) {
    case (acc, c) if c.isUpper => acc + 60 + (c - 'A')
    case (acc, c) if c.isLower => acc + 60 + (c - 'a')
    case (acc, _) => acc
  }
}

case class Edge(pre: String, post: String) {
  val toSet = Set(pre, post)
}