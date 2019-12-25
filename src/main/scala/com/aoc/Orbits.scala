package com.aoc

import scala.annotation.tailrec
import scala.collection.{mutable => m}

object Orbits {
  def countOrbits(inputLines: Seq[String]): Int =
    new Orbits(inputLines).countOrbits
}

class Orbits(inputLines: Seq[String]) {
  lazy val tree = new m.TreeMap[String, String]()
  fillInTree()

  /**
   * We just calculate the length of the path and subtract 2.
   */
  def calcPathToSanta(start: String = "YOU"): Int = {
    val rootToSanta = pathToRoot("SAN")
    val rootToMe = pathToRoot("YOU")
    val santaFork = rootToSanta.dropWhile(rootToMe.contains)
    val myFork = rootToMe.reverse.takeWhile { n => !rootToSanta.contains(n) }
      santaFork.length + myFork.length - 2
  }

  def countOrbits: Int = tree.keys.toSeq.map(calcDepth(_)).sum

  @tailrec
  final def pathToRoot(k: String, nodes: Seq[String] = Seq()): Seq[String] = {
    val letter = tree.get(k)
    if (letter.isEmpty) (k +: nodes) else pathToRoot(letter.get, k +: nodes)
  }

  @tailrec
  final def calcDepth(k: String, n: Int = 0): Int = {
    val letter = tree.get(k)
    if (letter.isEmpty) n else calcDepth(letter.get, n + 1)
  }

  def fillInTree() = inputLines foreach processString

  def processString(s: String): Unit = {
    val split = s.split("\\)")
    tree(split.last) = split.head
  }
}
