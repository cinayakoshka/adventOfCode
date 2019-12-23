package com.aoc

import org.scalatest.funspec.AnyFunSpec

import org.scalatest.matchers.should.Matchers

class Day4Test extends AnyFunSpec  with Matchers {
  describe("part 1") {
    val tester = new Day4()
    import tester._

    it("computes hasRepeat") {
      hasRepeat("123456") shouldBe false
      hasRepeat("11234") shouldBe true
      hasRepeat("111111") shouldBe true
    }

    it("detectsDecreasing") {
      isDecreasing("12343") shouldBe true
      isDecreasing("11111111") shouldBe false
      isDecreasing("111111") shouldBe false
    }

    it("passes provided cases") {
      isValid(111111) shouldBe true
      isValid(223450) shouldBe false
      isValid(123789) shouldBe false
    }

    it("small valid in range") {
      val small = new Day4(10, 12)
      small.validInRange() shouldBe 1
    }

    it("answer the question") {
      val answer = new Day4(256310, 732736)
      answer.validInRange() shouldBe 979
    }
  }

  describe("part 2") {
    val tester = new Day4Part2()
    import tester._

    it("meets new criteria") {
      hasRepeat("11") shouldBe true
      hasRepeat("111") shouldBe false
      hasRepeat("11222") shouldBe true
    }

    it("answer the question") {
      val answer = new Day4Part2(256310, 732736)
      answer.validInRange() shouldBe 635
    }
  }
}
