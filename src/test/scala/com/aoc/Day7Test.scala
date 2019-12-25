package com.aoc

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source

class Day7Test  extends AnyFunSpec with Matchers {
  private def source = Source.fromURL(getClass.getResource("/day7Input.txt"))
  private val inits = Seq(0, 1, 2, 3, 4)
  describe("part 1") {
    it("solves the first toy problem") {
      val toy = Seq(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
      IntCodeReader.amplifySequence(inits.reverse, toy) shouldBe 43210
      IntCodeReader.maxAmplificationSequence(inits, toy) shouldBe (Seq(4,3,2,1,0), 43210)
    }

    it("solves the second toy problem") {
      val toy = Seq(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0)
      IntCodeReader.maxAmplificationSequence(inits, toy) shouldBe (Seq(4,3,2,1,0).reverse, 54321)
    }

    it("solves the third toy problem") {
      val toy = Seq(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)
      IntCodeReader.maxAmplificationSequence(inits, toy) shouldBe (Seq(1,0,4,3,2), 65210)
    }

    it("answer") {
      IntCodeReader
        .maxAmplificationSequence(
          inits,
          source.getLines().next().split(",").map(_.toInt)
        ) shouldBe (Seq(2, 1, 4, 3, 0), 298586)
    }
  }
}
