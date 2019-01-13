package day19

import org.scalatest.FlatSpec

class Day19Spec extends FlatSpec {

  val sampleProgram = Program(0, Vector(
    Instruction(SetI, 5, 0, 1),
    Instruction(SetI, 6, 0, 2),
    Instruction(AddI, 0, 1, 0),
    Instruction(AddR, 1, 2, 3),
    Instruction(SetR, 1, 0, 0),
    Instruction(SetI, 8, 0, 4),
    Instruction(SetI, 9, 0, 5)
  ))

  "Program" should "parse a program" in {
    val input =
      """#ip 0
        |seti 5 0 1
        |seti 6 0 2
        |addi 0 1 0
        |addr 1 2 3
        |setr 1 0 0
        |seti 8 0 4
        |seti 9 0 5
      """.stripMargin

    val program = Program.parse(input)
    assertResult(sampleProgram)(program)
  }

  it should "run a program" in {
    val finalState = sampleProgram.run()
    assertResult(State(7,5,6,0,0,9))(finalState)
  }
}
