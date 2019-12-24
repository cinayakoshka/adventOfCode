package com.aoc

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source

class Day5Test extends AnyFunSpec with Matchers with Utils {
  import IntCodeReader._
  describe("part 1") {
    it("some basics") {
      readImmediateModes("002", 2) shouldBe Seq(false, false)
      readImmediateModes("199", 2) shouldBe Seq(true, false)
      readImmediateModes("1055", 3) shouldBe Seq(false, true, false)
    }

    it("correct output from given minitest") {
      IntCodeReader.read(Seq(1002,4,3,4,33)) shouldBe Seq(1002,4,3,4,99)
      IntCodeReader.read(Seq(1101,100,-1,4,0)) shouldBe Seq(1101,100,-1,4,99)
    }

    it("answer") {
      val source = Source.fromURL(getClass.getResource("/day5Input.txt"))
      val raw = source.getLines().next().split(",").map(_.toInt)
      new IntCodeReader(raw).test() shouldBe 11933517
    }
  }

  describe("part 2") {
    
    it("compares things to 8 - equality") {
      // 3,9,8,9,10,9,4,9,99,-1,8 - Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
      val reader = new IntCodeReader(Seq(3,9,8,9,10,9,4,9,99,-1,8))
      reader.test(8) shouldBe 1
      reader.test(7) shouldBe 0
    }

    it("compares things to 8 - lt") {
      // 3,9,7,9,10,9,4,9,99,-1,8 - Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
      val reader = new IntCodeReader(Seq(3,9,7,9,10,9,4,9,99,-1,8))
      reader.test(7) shouldBe 1
      reader.test(9) shouldBe 0
      reader.test(8) shouldBe 0
    }

    it("compares things to 8 - equality + immediate") {
      // 3,3,1108,-1,8,3,4,3,99 - Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
      val reader = new IntCodeReader(Seq(3,3,1108,-1,8,3,4,3,99))
      reader.test(8) shouldBe 1
      reader.test(7) shouldBe 0
      reader.test(9) shouldBe 0
    }

    it("compares things to 8 - lt + immediate") {
      // 3,3,1107,-1,8,3,4,3,99 - Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
      val reader = new IntCodeReader(Seq(3,3,1107,-1,8,3,4,3,99))
      reader.test(7) shouldBe 1
      reader.test(9) shouldBe 0
      reader.test(8) shouldBe 0
    }

    it("the answer") {
      val source = Source.fromURL(getClass.getResource("/day5Input.txt"))
      val raw = source.getLines().next().split(",").map(_.toInt)
      new IntCodeReader(raw).test(5) shouldBe 10428568
    }
  }
}
