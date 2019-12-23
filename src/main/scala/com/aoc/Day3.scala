package com.aoc

import com.aoc.Day3.Wire

import scala.collection.{ mutable => m}

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

object Day3 {
  type Wire = Seq[String]
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
      val steps = acc.stepTo(step)
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

object Day3Part2 {
  case class Visit(label: Int = 0, steps: Int = 0)
}

class Day3Part2(rawWires: Seq[Wire]) {
  import Day3Part2._

  lazy val grid: m.Map[Point, Set[Visit]] = new m.HashMap()

  val wires: Seq[Seq[Step]] = rawWires.map(_.map(Direction.parse))
  wires.zipWithIndex.map { case (w, i) => addWire(i, w) }

  def getManhattanDistance(p: Point): Int = Math.abs(p.x) + Math.abs(p.y)

  def closestIntersection = findNearestIntersection()._2.map(_.steps).sum

  def findNearestIntersection(): (Point, Set[Visit]) = {
    val possibles = grid.filter { case (p, ws) => ws.size == wires.length  && p != Point(0, 0)}
      if (possibles.nonEmpty) {
        possibles.minBy(_._2.map(_.steps).sum)
      } else throw new IllegalStateException("no intersection to find.")
  }

  def addWire(label: Int, wire: Seq[Step]): Unit = {
    wire.foldLeft((Point(), 0)) { (acc: (Point, Int), step: Step) => {
      val steps = acc._1.stepTo(step)
      val marks = acc._2 to acc._2 + steps.length
      val zipped = steps.zip(marks)
      zipped.foreach {
        case (point, i) => addPoint(label, point, i)
      }
      zipped.last
    }
    }
  }

  def addPoint(label: Int, p: Point, steps: Int): Unit = {
    val existing: Option[Set[Visit]] = grid.get(p)
    if (existing.isEmpty) {
      grid(p) = Set(Visit(label, steps))
    } else existing.foreach { visits =>
        if (!visits.exists(_.label == label)) {
          grid(p) = visits + Visit(label, steps)
        }
      }
  }
}
