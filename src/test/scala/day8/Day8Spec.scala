package day8

import org.scalatest.FlatSpec

class Day8Spec extends FlatSpec {
  val testInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
  val challenge = testInput.split("""\s+""").map(_.toInt).toList

  "Day8.parse" should "parse an input into a Node" in {
    val nodeD = Node(Vector.empty, Vector(Metadata(99)))
    val nodeC = Node(Vector(nodeD), Vector(Metadata(2)))
    val nodeB = Node(Vector.empty, Vector(10,11,12).map(Metadata))
    val nodeA = Node(Vector(nodeB, nodeC), Vector(1,1,2).map(Metadata))

    Day8.parseNode(challenge) match {
      case None => fail("Parse failed")
      case Some(node) =>
        assertResult(nodeA)(node)
        assertResult(138)(node.metadataChecksum)
        assertResult(66)(node.eval)
    }
  }
}
