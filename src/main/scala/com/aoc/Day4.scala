package com.aoc

/**
  * The password:
  * 
  * It is a six-digit number.
  * The value is within the range given in your puzzle input.
  * Two adjacent digits are the same (like 22 in 122345).
  * Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
  * Other than the range rule, the following are true:
  * 
  * 111111 meets these criteria (double 11, never decreases).
  * 223450 does not meet these criteria (decreasing pair of digits 50).
  * 123789 does not meet these criteria (no double).
  * How many different passwords within the range given in your puzzle input meet these criteria?
  */
class Day4(l: Int = 99999, r: Int = 1000000) {
  val left: Int = Seq(l, 100000).max
  val right: Int = Seq(r, 999999).min

  def isValid(i: Int): Boolean = isInRange(i) && stringTests(i.toString)

  def isInRange(i: Int): Boolean = left <= i && i <= right

  def validInRange(): Int = (left to right).count { i =>
    stringTests(i.toString)
  }

  def stringTests(s: String): Boolean = hasRepeat(s) && !isDecreasing(s)

  def hasRepeat(s: String): Boolean = s.zip(s.tail).exists { case (a, b) => a == b }

  def isDecreasing(s: String): Boolean = s.zip(s.tail).exists {
    case (a, b) => a.toInt > b.toInt
  }
}

class Day4Part2(l: Int = 99999, r: Int = 1000000) extends Day4(l, r) {
  case class CCount(char: Char, count: Int)
  override def hasRepeat(s: String): Boolean = countRepeats(s).exists(_.count == 2)

  def countRepeats(s: String): Seq[CCount] = s.foldLeft(Seq.empty[CCount]) {
    case (acc, c) if acc.isEmpty => Seq(CCount(c, 1))
    case (acc, c) if acc.head.char == c => acc.head.copy(count = acc.head.count + 1) +: acc.tail
    case (acc, c) => CCount(c, 1) +: acc
  }

}