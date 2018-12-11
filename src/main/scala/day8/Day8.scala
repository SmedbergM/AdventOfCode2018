package day8

import scala.annotation.tailrec
import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import common.AdventApp
import scalaj.http.Http

object Day8Part1 extends AdventApp {
  val challenge = Day8.fetchChallenge(sessionId)

  val optNode = Day8.parseNode(challenge)
  println(s"Task 1: ${optNode.map(_.metadataChecksum)}")
  println(s"Task 2: ${optNode.map(_.eval)}")
  println(s"Task 2, second try ${optNode.map(_.eval2)}")
}

object Day8 extends LazyLogging {
  def fetchChallenge(sessionId: String): List[Int] = {
    val url = "https://adventofcode.com/2018/day/8/input"
    val splitter = """\s+""".r
    val request = Http(url)
      .cookie("session", sessionId)
    request.asString.body.split(splitter.regex).flatMap(parseInt).toList
  }

  def parseNode(encoded: List[Int]): Option[Node] = {
    def parseIter(xs: List[Int], stack: Stack): Option[Node] = (xs, stack) match {
      case (nodeCount :: metadataCount :: nextXs, Empty :: Nil) =>
        parseIter(nextXs, PartialNode(Vector.empty, nodeCount, metadataCount) :: Nil)
      case (Nil, CompleteNode(node) :: Nil) => Some(node)
      case (_, CompleteNode(node) :: PartialNode(childNodes, remainingChildNodes, expectedMetadataCount) :: rest) =>
        val nextStack = PartialNode(childNodes :+ node, remainingChildNodes - 1, expectedMetadataCount) :: rest
        parseIter(xs, nextStack)
      case (Nil, _) =>
        logger.error(s"End-of-input reached with parsing incomplete: ${stack}")
        None

      case (_, PartialNode(_, 0, metadataCount) :: _) if xs.length < metadataCount =>
        logger.error(s"Ran out of metadata; needed ${metadataCount}, only have ${xs.length}")
        None
      case (_, PartialNode(_, 0, metadataCount) :: _) if metadataCount < 0 =>
        logger.error(s"Cannot extract ${metadataCount} metadata")
        None
      case (_, PartialNode(childNodes, 0, metadataCount) :: rest) =>
        val (metadata, nextXs) = xs.splitAt(metadataCount)
        val node = Node(childNodes, metadata.map(Metadata).toVector)
        parseIter(nextXs, CompleteNode(node) :: rest)
      case (childNodeCount :: childMetadataCount :: nextXs, (pn@PartialNode(_, remainingChildNodes, _)) :: rest)
        if remainingChildNodes > 0 =>

        val nextStack = PartialNode(Vector.empty, childNodeCount, childMetadataCount) :: pn :: rest
        parseIter(nextXs, nextStack)
      case (_, PartialNode(_, remainingChildNodes, _) :: _) =>
        // Then either remainingChildNodes is negative, or xs.length < 2
        logger.error(s"Cannot parse ${remainingChildNodes} remaining children from ${xs.length} data")
        None
    }

    parseIter(encoded, Empty :: Nil)
  }

  sealed trait ParserStatus

  case class PartialNode(childNodes: Vector[Node], remainingChildNodes: Int, expectedMetadataCount: Int) extends ParserStatus

  case class CompleteNode(node: Node) extends ParserStatus

  case object Empty extends ParserStatus

  type Stack = List[ParserStatus]

  private def parseInt(s: String): Option[Int] = Try(Integer.parseInt(s)).toOption
}

case class Metadata(value: Int)

case class Node(childNodes: Vector[Node],
                metadata: Vector[Metadata]) {

  def metadataChecksum: Int = mdChecksum(0, Nil)

  @tailrec
  protected final def mdChecksum(acc: Int, togo: List[List[Node]]): Int = {
    def nextAcc = acc + metadata.map(_.value).sum
    childNodes.toList match {
      case Nil =>
        togo match {
          case Nil => nextAcc
          case Nil :: toGoRest =>
            mdChecksum(acc, toGoRest)
          case (toGoHead :: toGoOther) :: toGoRest =>
            toGoHead.mdChecksum(nextAcc, toGoOther :: toGoRest)
        }
      case headChildNode :: restChildNodes =>
        headChildNode.mdChecksum(nextAcc, restChildNodes :: togo)
    }
  }

  private def _eval: Int = if (childNodes.isEmpty) {
    metadataChecksum
  } else {
    metadata.collect {
      case Metadata(idx) if 1 <= idx && idx <= childNodes.length =>
        val childNode = childNodes(idx - 1)
        childNode._eval
    }.sum
  }

  def eval: Int = evalIter(0, Nil)
  def eval2: Int = evalIter2(0, 1, Nil)

  @tailrec
  protected final def evalIter(acc: Int, togo: List[List[Node]]): Int = {
    def nextToGo = metadata.collect {
      case Metadata(idx) if childNodes.isDefinedAt(idx - 1) => childNodes(idx - 1)
    }.toList

    if (childNodes.isEmpty) {
      def nextAcc = acc + metadata.map(_.value).sum // * multiplier
      togo match {
        case Nil => nextAcc
        case Nil :: rest =>
          evalIter(acc, rest)
        case (head :: headRest) :: rest =>
          head.evalIter(nextAcc, headRest :: rest)
      }
    } else (nextToGo, togo) match {
      case (Nil, Nil) => acc
      case (Nil, Nil :: rest) =>
        evalIter(acc, rest)
      case (Nil, (head :: headRest) :: rest) =>
        head.evalIter(acc, headRest :: rest)
      case (head :: rest, _) =>
        head.evalIter(acc, rest :: togo)
    }
  }

  // TODO: Doesn't work
  @tailrec
  protected final def evalIter2(acc: Int, multiplier: Int, togo: List[List[(Int, Node)]]): Int = {
    def nextTogos = metadata.collect {
      case Metadata(idx1) if childNodes.isDefinedAt(idx1 - 1) => idx1 - 1
    }.groupBy(identity).map { case (idx, xs) =>
      (multiplier * xs.length) -> childNodes(idx)
    }.toList

    if (childNodes.isEmpty) {
      def nextAcc = acc + metadata.map(_.value).sum * multiplier
      togo match {
        case Nil => nextAcc
        case Nil :: rest =>
          evalIter2(acc, multiplier, rest)
        case ((m, head) :: headRest) :: rest =>
          head.evalIter2(nextAcc, m, headRest :: rest)
      }
    } else (nextTogos, togo) match {
      case (Nil, Nil) => acc
      case (Nil, Nil :: rest) =>
        evalIter2(acc, multiplier, rest)
      case (Nil, ((m, head) :: headRest) :: rest) =>
        head.evalIter2(acc, m, headRest :: rest)
      case ((m, head) :: rest, _) =>
        head.evalIter2(acc, m, rest :: togo)
    }
  }
}
