package com.aoc

import scala.annotation.tailrec
import scala.collection.{mutable => m}

object IntCodeReader {
  type IntCode = m.ArraySeq[Int]
  // intermediate result, initializes to ret = 1
  case class State(input: Int = 1, place: Option[Int] = Some(0))
  def read(codes: Seq[Int]): IntCode = new IntCodeReader(codes).readToStop()
  def test(init: Int = 1, codes: Seq[Int]): Int = new IntCodeReader(codes).test(init)

  def readImmediateModes(s: String, l: Int): Seq[Boolean] = {
    val modes = s.dropRight(2)
    val padded: String = ("0" * (l - modes.length)) ++ modes
    padded.map(_ == '1').reverse
  }
}

class IntCodeReader(rawCodes: Seq[Int]) {
  import IntCodeReader._

  val intCode: IntCode = m.ArraySeq(rawCodes: _*)

  def readToStop(init: Int = 1): IntCode = {
    readToStopRec(State(init))
    intCode
  }

  def test(init: Int = 1): Int = {
    readToStopRec(State(init)).input
  }

  @tailrec
  final def readToStopRec(state: State): State = {
    if (state.place.isEmpty) state else {
      readToStopRec(doOpCode(state))
    }
  }

  def doOpCode(state: State): State = {
    val start = state.place.get
    intCode(start).toString match {
      case o if o.endsWith("1")  =>
        opCode1(start, readImmediateModes(o, 2))
        State(state.input, Some(start + 4))
      case o if o.endsWith("2")  =>
        opCode2(start, readImmediateModes(o, 2))
        State(state.input, Some(start + 4))
      case o if o.endsWith("3")  => opCode3(start + 1, readImmediateModes(o, 1))(state.input)
        State(state.input, Some(start + 2))
      case o if o.endsWith("4")  =>
        State(opCode4(start + 1, readImmediateModes(o, 1).head), Some(start + 2))
      case o if o.endsWith("5") =>
          state.copy(place = Some(opCode5(start + 1, readImmediateModes(o, 2))))
      case o if o.endsWith("6") =>
        state.copy(place = Some(opCode6(start + 1, readImmediateModes(o, 2))))
      case o if o.endsWith("7") => opCode7(start, readImmediateModes(o, 2))
        state.copy(place = Some(start + 4))
      case o if o.endsWith("8") => opCode8(start, readImmediateModes(o, 2))
        state.copy(place = Some(start + 4))
      case o if o.endsWith("99")  => State(state.input, None)
      case e  => throw new IllegalStateException(s"illegal ops code oh no! ($e at $start)")
    }
  }

  def getParameter(place: Int, isImmediateMode: Boolean): Int = {
    try {
      if (isImmediateMode) intCode(place) else intCode(intCode(place))
    } catch {
      case e: ArrayIndexOutOfBoundsException =>
        println(s"caught array error at $place / $isImmediateMode")
        println(s"current state: $intCode")
        throw e
    }
  }

  def storeParameter(place: Int, value: Int): Unit = {
    try {
      intCode(intCode(place)) = value
    } catch {
      case e: ArrayIndexOutOfBoundsException =>
        println(s"caught array error writing to $place")
        println(s"current state: $intCode")
        throw e
    }
  }

  def opCode2Params(place: Int, isImmediateModes: Seq[Boolean]): (Int, Int) = {
    val a = getParameter(place + 1, isImmediateModes.head)
    val b = getParameter(place + 2, isImmediateModes.last)
    (a, b)
  }

  def opCode1(place: Int, isImmediateModes: Seq[Boolean]): Unit = {
    val (a, b) = opCode2Params(place, isImmediateModes)
    intCode(intCode(place + 3)) = a + b
  }

  def opCode2(place: Int, isImmediateModes: Seq[Boolean]): Unit = {
    val (a, b) = opCode2Params(place, isImmediateModes)
    intCode(intCode(place + 3)) = a * b
  }

  /*
   * Opcode 3 takes a single integer as input and saves it to the
   * position given by its only parameter. For example, the
   * instruction 3,50 would take an input value and store it at
   * address 50.  
   */
  def opCode3(place: Int, isImmediateModes: Seq[Boolean])(intput: Int): Unit =
    intCode(intCode(place)) = intput

  /* Opcode 4 outputs the value of its only
   * parameter. For example, the instruction 4,50 would output the
   * value at address 50.
   */
  def opCode4(place: Int, isImmediateMode: Boolean): Int =
    getParameter(place, isImmediateMode)

  /*
   * Opcode 5 is jump-if-true: if the first parameter is non-zero, it
   * sets the instruction pointer to the value from the second
   * parameter. Otherwise, it does nothing.
   */
  def opCode5(place: Int, isImmediateModes: Seq[Boolean]): Int =
    if (getParameter(place, isImmediateModes.head) == 0) place + 2
    else getParameter(place + 1, isImmediateModes.last)

  /*
   * Opcode 6 is jump-if-false: if the first parameter is zero, it
   * sets the instruction pointer to the value from the second
   * parameter. Otherwise, it does nothing.
   */
  def opCode6(place: Int, isImmediateModes: Seq[Boolean]): Int =
    if (getParameter(place, isImmediateModes.head) != 0) place + 2
    else getParameter(place + 1, isImmediateModes.last)

  /*
   * Opcode 7 is less than: if the first parameter is less than the
   * second parameter, it stores 1 in the position given by the third
   * parameter. Otherwise, it stores 0.
   */
  def opCode7(place: Int, isImmediateModes: Seq[Boolean]): Unit = {
    val (a, b) = opCode2Params(place, isImmediateModes)
    val v = if (a < b) 1 else 0
    intCode(intCode(place + 3)) = v
  }

  /*
   * Opcode 8 is equals: if the first parameter is equal to the second
   * parameter, it stores 1 in the position given by the third
   * parameter. Otherwise, it stores 0.
   */
  def opCode8(place: Int, isImmediateModes: Seq[Boolean]): Unit = {
    val (a, b) = opCode2Params(place, isImmediateModes)
    val v = if (a == b) 1 else 0
    intCode(intCode(place + 3)) = v
  }
}
