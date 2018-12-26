package day16

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.util.Try
import scala.collection.mutable.{ArrayBuffer, HashSet => MHashSet}

import com.typesafe.scalalogging.LazyLogging
import common.AdventApp
import scalaj.http.Http

object Day16Part1 extends AdventApp with LazyLogging {
  val (samples, instructions) = Day16.fetchChallenge(sessionId)

  logger.info(s"Parsed ${samples.size} samples and ${instructions.size} instructions")
  val multiValent = samples.filter { sample =>
    Op.values.count(sample.matches) >= 3
  }
  logger.info(s"Your challenge contains ${multiValent.size} multivalent samples")

  val mappedOpCodes = Day16.identifyOpcodes(samples)
  logger.info(s"Mapped opcodes: ${mappedOpCodes}")

  val finalState = Day16.run(instructions, mappedOpCodes.get)
  logger.info(s"Final state: $finalState")
}

object Day16 extends LazyLogging {
  def fetchChallenge(sessionId: String): (Set[Sample], Seq[Instruction]) = {
    val url = "https://adventofcode.com/2018/day/16/input"
    val request = Http(url).cookie("session", sessionId)
    val response = request.asString
    parseChallenge(response.body)
  }

  def parseChallenge(body: String): (Set[Sample], Seq[Instruction]) = {
    val samples = MHashSet.empty[Sample]
    val instructions = ArrayBuffer.empty[Instruction]
    val beforePat = """Before: \[(\d+), (\d+), (\d+), (\d+)\]""".r
    def parseBefore(line: String): Option[State] = line match {
      case beforePat(sa, sb, sc, sd) => for {
        r0 <- parseByte(sa)
        r1 <- parseByte(sb)
        r2 <- parseByte(sc)
        r3 <- parseByte(sd)
      } yield State(r0, r1, r2, r3)
      case _ => None
    }
    val instructionPat = """(\d+) (\d+) (\d+) (\d+)""".r
    def parseInstruction(line: String): Option[Instruction] = line match {
      case instructionPat(sop, sa, sb, sc) => for {
        opCode <- parseByte(sop)
        a <- parseByte(sa)
        b <- parseByte(sb)
        c <- parseByte(sc)
      } yield Instruction(opCode, a, b, c)
      case _ => None
    }
    val afterPat = """After:  \[(\d+), (\d+), (\d+), (\d+)\]""".r
    def parseAfter(line: String): Option[State] = line match {
      case afterPat(a, b, c, d) => for {
        r0 <- parseByte(a)
        r1 <- parseByte(b)
        r2 <- parseByte(c)
        r3 <- parseByte(d)
      } yield State(r0, r1, r2, r3)
    }

    def iter(lines: List[String]): Unit = {
      @tailrec
      def iterSamples(lines2: List[String]): Unit = lines2 match {
        case Nil | _ :: Nil =>
          logger.warn("EOF encountered without parsing any instructions!")
        case h0 :: h1 :: rest if h0.forall(_.isWhitespace) && h1.forall(_.isWhitespace) =>
          iterInstructions(rest)
        case h :: rest if h.forall(_.isWhitespace) =>
          iterSamples(rest)
        case line0 :: line1 :: line2 :: rest =>
          val optSample = for {
            before <- parseBefore(line0)
            instruction <- parseInstruction(line1)
            after <- parseAfter(line2)
          } yield Sample(before, instruction, after)
          optSample match {
            case Some(sample) =>
              samples.add(sample)
              iterSamples(rest)
            case None =>
              logger.warn("Unexpected lines not parsable as Sample:")
              logger.warn(line0)
              logger.warn(line1)
              logger.warn(line2)
              iterSamples(line1 :: line2 :: rest)
          }
        case h :: rest =>
          logger.warn(s"Unexpected line encountered:")
          logger.warn(h)
          iterSamples(rest)
      }
      @tailrec
      def iterInstructions(lines2: List[String]): Unit = lines2 match {
        case Nil => //
        case h :: rest =>
          parseInstruction(h).foreach(instruction => instructions.append(instruction))
          iterInstructions(rest)
      }

      iterSamples(lines)
    }

    iter(body.split("\n").toList)

    (samples.toSet, instructions.toVector)
  }

  def identifyOpcodes(samples: Set[Sample]): Option[Map[Byte, Op]] = {
    val candidates = TrieMap.empty[Byte, Set[Op]]
    samples.foreach { sample =>
      val opCode = sample.instruction.opCode
      val ops = candidates.getOrElse(opCode, Op.values)
      val nextOps = ops.filter(sample.matches)
      candidates.put(opCode, nextOps)
    }

    def iter(identified: Map[Byte, Op]): Option[Map[Byte, Op]] = {
      val newlyIdentified = candidates.foldLeft(Set.empty[Byte]){ case (acc, (opCode, ops)) =>
        val nextOps = ops -- identified.filterKeys(_ != opCode).values
        candidates.put(opCode, nextOps)
        if (ops.size > 1 && nextOps.size == 1) {
          acc + opCode
        } else {
          acc
        }
      }
      if (newlyIdentified.isEmpty && identified.size == candidates.size) {
        Some(identified)
      } else if (newlyIdentified.isEmpty) {
        logger.warn(s"Cannot resolve some opCodes: ambiguous codes ${candidates.collect{
          case (opCode, ops) if ops.size != 1 => s"${opCode} : ${ops.mkString(",")}"
        }}")
        None
      } else {
        iter(identified ++ newlyIdentified.map(opCode => opCode -> candidates(opCode).head))
      }
    }

    val identified = candidates.collect {
      case (k, ops) if ops.size == 1 => k -> ops.head
    }.toMap
    iter(identified)
  }

  def run(instructions: Seq[Instruction], mappedOpCodes: Map[Byte, Op]): State = {
    instructions.foldLeft(State(0,0,0,0)){ case (state, Instruction(opCode, a, b, c)) =>
      mappedOpCodes(opCode).execute(state, a, b, c)
    }
  }

  private def parseByte(s: String): Option[Byte] = Try(Integer.parseInt(s).toByte).toOption

}

case class State(r0: Long, r1: Long, r2: Long, r3: Long) {
  def getWord(r: Byte): Long = r match {
    case 0 => r0
    case 1 => r1
    case 2 => r2
    case 3 => r3
    case _ => throw new IllegalArgumentException
  }

  def setWord(r: Byte, v: Long): State = r match {
    case 0 => copy(r0 = v)
    case 1 => copy(r1 = v)
    case 2 => copy(r2 = v)
    case 3 => copy(r3 = v)
    case _ => throw new IllegalArgumentException
  }
}

case class Instruction(opCode: Byte, a: Long, b: Long, c: Long)

case class Sample(before: State, instruction: Instruction, after: State) {
  def matches(op: Op): Boolean = op.execute(before, instruction.a, instruction.b, instruction.c) == after
}

sealed trait Op {
  def execute(state: State, a: Long, b: Long, c: Long): State
}

object Op {
  val values: Set[Op] = Set(AddR, AddI,
    MulR, MulI,
    BanR, BanI,
    BorR, BorI,
    SetR, SetI,
    GtIR, GtRI, GtRR,
    EqIR, EqRI, EqRR
  )
}

case object AddR extends Op {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setWord(c, state.getWord(a) + state.getWord(b))
  }
}

case object AddI extends Op {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setWord(c, state.getWord(a) + b)
  }
}

case object MulR extends Op {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setWord(c, state.getWord(a) * state.getWord(b))
  }
}

case object MulI extends Op {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setWord(c, state.getWord(a) * b)
  }
}

case object BanR extends Op {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setWord(c, state.getWord(a) & state.getWord(b))
  }
}

case object BanI extends Op {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setWord(c, state.getWord(a) & b)
  }
}

case object BorR extends Op {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setWord(c, state.getWord(a) | state.getWord(b))
  }
}

case object BorI extends Op {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setWord(c, state.getWord(a) | b)
  }
}

case object SetR extends Op {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setWord(c, state.getWord(a))
  }
}

case object SetI extends Op {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setWord(c, a)
  }
}

case object GtIR extends Op {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setWord(c, a > state.getWord(b))
  }
}

case object GtRI extends Op {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setWord(c, state.getWord(a) > b)
  }
}

case object GtRR extends Op {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setWord(c, state.getWord(a) > state.getWord(b))
  }
}

case object EqIR extends Op {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setWord(c, a == state.getWord(b))
  }
}

case object EqRI extends Op {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setWord(c, b == state.getWord(a))
  }
}

case object EqRR extends Op {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setWord(c, state.getWord(a) == state.getWord(b))
  }
}