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
   * @param start
   * @return
   */
  def calcPathToSanta(start: String = "YOU"): Option[Int] =
    ascendToSanta(Seq[String](), start, 0).map(_ - 2)

  @tailrec
  final def ascendToSanta(exclude: Seq[String], node: String, n: Int): Option[Int] = node match {
    case "SAN" => Some(n)
    case "COM" => None
    case k =>
      val descent: Option[Int] = descendToSanta(Seq(node), Seq(node), n)
      if (descent.isDefined) descent else {
        val p = tree.get(node)
        if (p.isEmpty) None else {
          ascendToSanta(node +: exclude, p.get, n + 1) }
        }
  }

  @tailrec
  final def descendToSanta(exclude: Seq[String], nodes: Seq[String], n: Int): Option[Int] =
    if (nodes.isEmpty) { None } else {
      if (nodes.contains("SAN")) Some(n) else {
        val nodesToDescend = tree.filter { case (k, v: String) =>
          !exclude.contains(k) && nodes.contains(v) }.keys
        descendToSanta(nodes ++ exclude, nodesToDescend.toSeq, n + 1)
      }
    }

  def countOrbits: Int = tree.keys.toSeq.map(calcDepth(_)).sum

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
