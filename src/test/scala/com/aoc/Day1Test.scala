package com.aoc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Day1Test extends AnyFunSuite with Matchers {
  import Day1._

  val fixtures: Seq[(Int, Int)] = Seq(
    (12, 2),
    (14, 2),
    (1969, 654),
    (100756, 33583)
  )

  test("calcFule") {
    fixtures foreach {
      case (init, ans) =>
        calcFuel(init) should be(ans)
    }
  }

  test("calcTotalFuel") {
    calcTotalFuel(fixtures.map(_._1)) should be(fixtures.map(_._2).sum)
  }

  test("calc additional fuel") {
    calcAdditionalFuel(1969) should be(966)
    calcAdditionalFuel(100756) should be(50346)
  }

}
