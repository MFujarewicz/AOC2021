package aoc2021

import toolbox.DataLoader

object Day5 extends App {
  val data = DataLoader(5)

  case class Line(x1: Int, y1: Int, x2: Int, y2: Int) {
    def allPoints = {
      val xs = if x1 < x2 then x1 to x2 else x1 to x2 by -1
      val ys = if y1 < y2 then y1 to y2 else y1 to y2 by -1
      xs.zipAll(ys, x1, y1)
    }
  }

  def countIntersections(lines: List[Line]) = {
    lines.flatMap(_.allPoints).groupMapReduce(identity)(_ => 1)(_ + _).values.count(_ > 1)
  }

  val lines = data.map({
    case s"${x1},${y1} -> ${x2},${y2}" => Line(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
  })

  val countHV = countIntersections(lines.filter(l => l.x1 == l.x2 || l.y1 == l.y2))
  val countAll = countIntersections(lines)

  println(countHV)
  println(countAll)
}
