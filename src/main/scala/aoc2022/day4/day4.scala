package aoc2022.day4

import toolbox.DataLoader

case class Range(from: Int, to: Int) {
  def contains(otherRange: Range): Boolean = {
    from <= otherRange.from && to >= otherRange.to
  }

  def overlaps(otherRange: Range): Boolean = {
    !notOverlaps(otherRange)
  }

  def notOverlaps(otherRange: Range): Boolean = {
    otherRange.from > to || otherRange.to < from
  }
}

object Range {
  def fromString(s: String): Range = {
    s match
      case s"$a-$b" => Range(a.toInt, b.toInt)
  }
}

@main def day4(): Unit = {

  val data: List[String] = DataLoader(4, 2022, useTestData = false)

  val rangePairs = data.map(line => line.split(",").map(s => Range.fromString(s))).map(l => (l(0), l(1)))

  val onlyContained = rangePairs.filter { case (range1: Range, range2: Range) => range1.contains(range2) || range2.contains(range1) }

  val overlapping = rangePairs.filter { case (range1: Range, range2: Range) => range1.overlaps(range2)}

  println(onlyContained.size)
  println(overlapping.size)
}
