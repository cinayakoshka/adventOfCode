package com.aoc

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source

class Day6Test extends AnyFunSpec with Matchers {
  import Orbits._
  def source = Source.fromURL(getClass.getResource("/day6Input.txt"))

    describe("part 1") {
      it("some basics") {
        val sample = Seq("COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L")
        countOrbits(sample) shouldBe(42)
      }

      it("answer") {
        countOrbits(source.getLines().toSeq) shouldBe 402879
      }
    }

  describe("part 2") {
    it("passes basic test") {
      val sample = Seq("COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L","K)YOU","I)SAN")
      val o = new Orbits(sample)
      o.calcPathToSanta("YOU") should be (Some(4))
    }

    it("answer") {
      new Orbits(source.getLines().toSeq).calcPathToSanta() shouldBe Some(5)
    }
  }
}

