package day16

import org.scalatest.FlatSpec

class Day16Spec extends FlatSpec {

  "sample.matches" should "match opcode results" in {
    val sample = Sample(
      before = State(3,2,1,1),
      instruction = Instruction(9,2,1,2),
      after = State(3,2,2,1)
    )
    assertResult(Set(MulR, AddI, SetI))(Op.values.filter(sample.matches))
  }
}
