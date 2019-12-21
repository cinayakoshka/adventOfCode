package com.aoc

import scala.collection.{mutable => m}

object Direction {
  val directions = Set(Up, Down, Left, Right)
  def fromChar(c: Char): Option[Direction] = directions.find(_.name == c)

  def parse(s: String): Step = {
    val dir = fromChar(s.head)
    val distance = s.tail.toInt
    dir
      .map(d => Step(d, distance))
      .getOrElse(throw new IllegalArgumentException(s"unable to parse step from $s"))
  }
}

trait Direction {
  val name: Char
}
case object Up extends Direction { val name = 'U' }
case object Down extends Direction { val name = 'D' }
case object Left extends Direction { val name = 'L' }
case object Right extends Direction { val name = 'R' }

case class Step(direction: Direction, distance: Int)
case class Point(x: Int = 0, y: Int = 0) {

  def stepTo(step: Step): Seq[Point] = step.direction match {
    case Up    => Point.flexibleRange(y, y + step.distance).map(ny => Point(x, ny))
    case Down  => Point.flexibleRange(y, y - step.distance).map(ny => Point(x, ny))
    case Left  => Point.flexibleRange(x, x - step.distance).map(nx => Point(nx, y))
    case Right => Point.flexibleRange(x, x + step.distance).map(nx => Point(nx, y))
  }
}

object Point {

  def flexibleRange(i: Int, j: Int): Seq[Int] = {
    if (i <= j) (i to j)
    else (j to i).reverse
  }
}

class Day3(rawWires: Seq[Seq[String]]) {
  lazy val grid: m.Map[Point, Set[Int]] = new m.HashMap()
  val wires: Seq[Seq[Step]] = rawWires.map(_.map(Direction.parse))
  wires.zipWithIndex.map { case (w, i) => addWire(i, w) }

  def closestIntersection: Int =
    findIntersections
      .map(getManhattanDistance)
      .filter(_ != 0)
      .minOption
      .getOrElse(throw new IllegalStateException("no intersections found"))

  def getManhattanDistance(p: Point): Int = Math.abs(p.x) + Math.abs(p.y)

  def findIntersections: Seq[Point] =
    grid.filter { case (p, ws) => ws.size == wires.length }.keys.toSeq

  def addWire(label: Int, wire: Seq[Step]): Unit =
    wire.foldLeft(Point()) { (acc: Point, step: Step) =>
      //println(s"calculating steps from $acc to $step")
      val steps = acc.stepTo(step)
      //println(s"last step is ${steps.last}")
      steps.foreach { p =>
        addPoint(label, p)
      }
      steps.last
    }

  def addPoint(label: Int, p: Point): Unit = {
    val existing = grid.getOrElse(p, Set())
    grid(p) = existing + label
  }

}
