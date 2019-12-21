package com.aoc

import scala.collection.{mutable => m}

object Day2 {
  type IntCode = m.ArraySeq[Int]
  def read(codes: Seq[Int]): IntCode = new Day2(codes).readToStop()
}

class Day2(rawCodes: Seq[Int]) {
  import Day2._

  val intCode: IntCode = m.ArraySeq(rawCodes: _*)

  def readToStop(): IntCode = {
    intCode.indices filter (_ % 4 == 0) takeWhile (i => intCode(i) != 99) foreach { i =>
      doOpCode(i)
    }
    intCode
  }

  def doOpCode(place: Int): Unit = intCode(place) match {
    // Opcode 1 adds together numbers read from two positions and stores the result in a third
    // position. The three integers immediately after the opcode tell you these three positions -
    // the first two indicate the positions from which you should read the input values, and the
    // third indicates the position at which the output should be stored.
    case 1  => opCode1(place)
    case 2  => opCode2(place)
    case 99 =>
    case e  => throw new IllegalStateException(s"illegal ops code oh no! ($e at $place)")
  }

  def opCode1(place: Int): Unit = {
    val a = intCode(intCode(place + 1)) + intCode(intCode(place + 2))
    intCode(intCode(place + 3)) = a
  }

  def opCode2(place: Int): Unit = {
    val a = intCode(intCode(place + 1)) * intCode(intCode(place + 2))
    intCode(intCode(place + 3)) = a
  }

}
