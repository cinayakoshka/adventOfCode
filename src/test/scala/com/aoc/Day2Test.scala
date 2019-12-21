package com.aoc

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable

import org.scalatest.matchers.should.Matchers

class Day2Test extends AnyFunSuite with Matchers {

  val testCases = Map(
    Seq(1, 0, 0, 0, 99) -> Seq(2, 0, 0, 0, 99),
    Seq(2, 3, 0, 3, 99) -> Seq(2, 3, 0, 6, 99),
    Seq(2, 4, 4, 5, 99, 0) -> Seq(2, 4, 4, 5, 99, 9801),
    Seq(1, 1, 1, 4, 99, 5, 6, 0, 99) -> Seq(30, 1, 1, 4, 2, 5, 6, 0, 99)
  )

  test("readRawIntCode") {
    val q = Seq(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50)
    val oracle = Seq(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
    val answer = Day2.read(q)
    answer should be(oracle)
  }

  test("more tests") {
    testCases foreach { case (q, a) => Day2.read(q) should be(a) }
  }

  val q1 = Seq(1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 13, 1, 19, 1, 10, 19, 23, 1, 23,
    9, 27, 1, 5, 27, 31, 2, 31, 13, 35, 1, 35, 5, 39, 1, 39, 5, 43, 2, 13, 43, 47, 2, 47, 10, 51, 1,
    51, 6, 55, 2, 55, 9, 59, 1, 59, 5, 63, 1, 63, 13, 67, 2, 67, 6, 71, 1, 71, 5, 75, 1, 75, 5, 79,
    1, 79, 9, 83, 1, 10, 83, 87, 1, 87, 10, 91, 1, 91, 9, 95, 1, 10, 95, 99, 1, 10, 99, 103, 2, 103,
    10, 107, 1, 107, 9, 111, 2, 6, 111, 115, 1, 5, 115, 119, 2, 119, 13, 123, 1, 6, 123, 127, 2, 9,
    127, 131, 1, 131, 5, 135, 1, 135, 13, 139, 1, 139, 10, 143, 1, 2, 143, 147, 1, 147, 10, 0, 99,
    2, 0, 14, 0)

  def testWithNounVerb(noun: Int, verb: Int): Seq[Int] = {
    val q = mutable.ArraySeq(q1: _*)
    q(1) = noun
    q(2) = verb
    Day2.read(q.toSeq).toSeq
  }

  //Once you have a working computer, the first step is to restore the gravity assist program
  //(your puzzle input) to the "1202 program alarm" state it had just before the last computer
  //caught fire. To do this, before running the program, replace position 1 with the value 12 and
  //replace position 2 with the value 2.
  ignore("answer the question") {
    val result = testWithNounVerb(12, 2)
    println(s"The answer is ${result.head}")
  }

  // determine what pair of inputs produces the output 19690720
  ignore("answer part 2") {
    import scala.util.Random
    val nouns: Seq[Int] = Random.shuffle(0 to 99)
    val verbs: Seq[Int] = Random.shuffle(0 to 99)
    val answer = nouns.find { noun: Int =>
      val verb: Option[Int] = verbs.find { verb: Int =>
        testWithNounVerb(noun, verb).head == 19690720
      }
      verb.foreach(v => println(s"found verb is $v"))
      verb.isDefined
    }
    println(s"The answer for part 2 is $answer")
    // 53 * 100 + 35 = 5335
  }

}
