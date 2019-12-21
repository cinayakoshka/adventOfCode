package com.aoc

import scala.annotation.tailrec
import scala.io.Source

/**
 * Fuel required to launch a given module is based on its mass. Specifically, to find the fuel
 * required for a module, take its mass, divide by three, round down, and subtract 2.
 *
 * For example:
 *
 * For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2.
 * For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2.
 * For a mass of 1969, the fuel required is 654.
 * For a mass of 100756, the fuel required is 33583.
 * The Fuel Counter-Upper needs to know the total fuel requirement. To find it, individually
 * calculate the fuel needed for the mass of each module (your puzzle input), then add together all
 * the fuel values.
 */
object Day1 extends Utils {
  def calcTotalFuel(masses: Seq[Int]): Int = masses.map(calcFuel).sum
  def calcFuel(mass: Int): Int = Math.max(0, (Math.floor(mass / 3.0) - 2).toInt)

  def answerProblem(filePath: String): Int = {
    val file = Source.fromFile(filePath)
    val masses = file.getLines.map(_.toInt).toSeq
    calcTotalFuel(masses)
  }

  // it's not unusual to kick off a recursive function with a small starter function.
  def calcAdditionalFuel(mass: Int): Int =
    calcAdditionalFuelRec(Seq(calcFuel(mass))).sum

  @tailrec
  def calcAdditionalFuelRec(fuels: Seq[Int]): Seq[Int] =
    if (fuels.head <= 2) {
      fuels
    } else calcAdditionalFuelRec(calcFuel(fuels.head) +: fuels)

  def answerProblem2(filePath: String): Int = {
    val file = Source.fromFile(filePath)
    val masses = file.getLines.map(_.toInt).toSeq
    masses.map(calcAdditionalFuel).sum
  }
}
