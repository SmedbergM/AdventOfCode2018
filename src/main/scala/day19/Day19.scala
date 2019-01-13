package day19

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import common.AdventApp
import day16.{narrow, toByte}
import scalaj.http.Http

object Day19Part1 extends AdventApp with LazyLogging {
  val program = Day19.fetchChallenge(sessionId)
  logger.info(s"I got a program with instruction register ${
    program.instructionRegister
  } and ${
    program.instructions.length
  } instructions")

  val finalState = program.run()

  logger.info(s"Final state: ${finalState}")
}

object Day19Part2 extends AdventApp with LazyLogging {
  // This takes a fuckton of time to run. The answer is the sum of the divisors of the number which gets initialized
  // into one of the registers (r3 in my challenge)
  val program = Day19.fetchChallenge(sessionId)

  val finalState = program.run(State.Zero.incrementRegister(0))

  logger.info(s"Final state: ${finalState}")
}

object Day19 {
  def fetchChallenge(sessionId: String): Program = {
    val url = "https://adventofcode.com/2018/day/19/input"
    val request = Http(url)
      .cookie("session", sessionId)
    val response = request.asString
    Program.parse(response.body)
  }
}

case class Program(instructionRegister: Byte, instructions: Vector[Instruction]) extends LazyLogging {
  def run(initialState: State = State.Zero): State = {
    @tailrec
    def iter(state: State): State = {
      val instructionPointer = state.getRegister(instructionRegister)
      if (instructionPointer < 0 || instructionPointer >= instructions.length) {
        state
      } else {
        val instruction = instructions(state.getRegister(instructionRegister).toInt)
        val nextState = instruction.execute(state).incrementRegister(instructionRegister)
        iter(nextState)
      }
    }

    iter(initialState)
  }
}

object Program {
  def parse(s: String): Program = {
    val ipPat = """#ip\s*(\d+)""".r
    val instrPat = """(\w{4})\s+(\d+)\s+(\d+)\s+(\d+)""".r

    val (optIP, instructions) = s.split("\n")
      .foldLeft(Option.empty[Byte], ArrayBuffer.empty[Instruction]) {
        case ((accIP, accInstrs), ipPat(ir)) =>
          Try(ir.toByte).toOption.orElse(accIP) -> accInstrs
        case ((accIP, accInstrs), instrPat(op, a, b, c)) =>
          accInstrs.append(Instruction(OpCode.opCodes(op), a.toByte, b.toByte, c.toByte))
          accIP -> accInstrs
        case (acc, _) => acc
      }

    Program(
      optIP.getOrElse(throw new IllegalArgumentException("This challenge must set the instruction pointer")),
      instructions.toVector
    )
  }
}

case class Instruction(opCode: OpCode, a: Byte, b: Byte, c: Byte) {
  def execute(state: State): State = opCode.execute(state, a, b, c)
}

case class State(r0: Long,
                 r1: Long,
                 r2: Long,
                 r3: Long,
                 r4: Long,
                 r5: Long) {

  def getRegister(b: Byte): Long = b match {
    case 0 => r0
    case 1 => r1
    case 2 => r2
    case 3 => r3
    case 4 => r4
    case 5 => r5
  }

  def setRegister(b: Byte, v: Long): State = b match {
    case 0 => copy(r0 = v)
    case 1 => copy(r1 = v)
    case 2 => copy(r2 = v)
    case 3 => copy(r3 = v)
    case 4 => copy(r4 = v)
    case 5 => copy(r5 = v)
  }

  def incrementRegister(b: Byte): State = setRegister(b, getRegister(b) + 1)
}

object State {
  val Zero = State(0,0,0,0,0,0)
}

sealed trait OpCode {
  def execute(state: State, a: Long, b: Long, c: Long): State
}

object OpCode {
  val opCodes: Map[String, OpCode] = Map(
    "addr" -> AddR,
    "addi" -> AddI,
    "mulr" -> MulR,
    "muli" -> MulI,
    "banr" -> BanR,
    "bani" -> BanI,
    "borr" -> BorR,
    "bori" -> BorI,
    "setr" -> SetR,
    "seti" -> SetI,
    "gtir" -> GtIR,
    "gtri" -> GtRI,
    "gtrr" -> GtRR,
    "eqir" -> EqIR,
    "eqri" -> EqRI,
    "eqrr" -> EqRR
  )
}

case object AddR extends OpCode {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setRegister(c, state.getRegister(a) + state.getRegister(b))
  }
}

case object AddI extends OpCode {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setRegister(c, state.getRegister(a) + b)
  }
}

case object MulR extends OpCode {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setRegister(c, state.getRegister(a) * state.getRegister(b))
  }
}

case object MulI extends OpCode {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setRegister(c, state.getRegister(a) * b)
  }
}

case object BanR extends OpCode {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setRegister(c, state.getRegister(a) & state.getRegister(b))
  }
}

case object BanI extends OpCode {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setRegister(c, state.getRegister(a) & b)
  }
}

case object BorR extends OpCode {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setRegister(c, state.getRegister(a) | state.getRegister(b))
  }
}

case object BorI extends OpCode {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setRegister(c, state.getRegister(a) | b)
  }
}

case object SetR extends OpCode {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setRegister(c, state.getRegister(a))
  }
}

case object SetI extends OpCode {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setRegister(c, a)
  }
}

case object GtIR extends OpCode {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setRegister(c, a > state.getRegister(b))
  }
}

case object GtRI extends OpCode {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setRegister(c, state.getRegister(a) > b)
  }
}

case object GtRR extends OpCode {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setRegister(c, state.getRegister(a) > state.getRegister(b))
  }
}

case object EqIR extends OpCode {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setRegister(c, a == state.getRegister(b))
  }
}

case object EqRI extends OpCode {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setRegister(c, state.getRegister(a) == b)
  }
}

case object EqRR extends OpCode {
  override def execute(state: State, a: Long, b: Long, c: Long): State = {
    state.setRegister(c, state.getRegister(a) == state.getRegister(b))
  }
}