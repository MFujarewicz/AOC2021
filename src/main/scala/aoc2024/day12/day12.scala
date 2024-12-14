package aoc2024.day12

import toolbox.DataLoader

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*
//import cats.implicits._

type Position = ((Int, Int), Char)

extension (p: Position) def x: Int = p._1._1
extension (p: Position) def y: Int = p._1._2
extension (p: Position) def name: Int = p._2


@main def day12(): Unit = {
  val usingTestData = false
  val input = DataLoader(12, 2024, useTestData = usingTestData).map(_.toVector).toVector

  val map = input.zipWithIndex.flatMap { case (v, x) => v.zipWithIndex.map { case (c, y) => ((x, y), c) } }.toMap

  enum Side:
    case Left, Right, Up, Down

  extension (p: Position) def neighbours: Seq[(Position, Side)] = List(
    ((p.x + 1, p.y), Side.Right),
    ((p.x - 1, p.y), Side.Left),
    ((p.x, p.y + 1), Side.Up),
    ((p.x, p.y - 1), Side.Down),
  ).map { case (n, side) => ((n, map.getOrElse(n, '1')), side) }


  val visited = mutable.Set[Position]()

  val scores = for (startingPos <- map.toList if !visited.contains(startingPos)) yield {
    var totalElems = 0
    var totalBorders = 0
    val currentName = startingPos.name
    val fences = mutable.Set.empty[(Position, Side)]
    var continuousFences = 0

    def survey(pos: Position): Unit = {
      if visited.contains(pos) || pos.name != currentName then return ()
      visited.add(pos)
      totalElems += 1
      pos.neighbours.foreach { case (n, side) =>
        if n.name == currentName then survey(n) else {
          fences.add((pos, side))
          totalBorders += 1
        }
      }
    }
    survey(startingPos)


    val HorizontalFences = fences.filter(a => a._2 == Side.Right || a._2 == Side.Left).groupBy(_._2).values.map(_.map(_._1)).flatMap(_.groupBy(_.x).values.map(_.map(_.y)))
    val VerticalFences = fences.filter(a => a._2 == Side.Up || a._2 == Side.Down).groupBy(_._2).values.map(_.map(_._1)).flatMap(_.groupBy(_.y).values.map(_.map(_.x)))

    (HorizontalFences ++ VerticalFences).map(_.toList.sorted).foreach(i => {
      val a = i.zip(i.drop(1))
      a.filter(t => t._2 - t._1 == 1).foreach(_ => continuousFences += 1)
    })


    (totalBorders * totalElems,
      (totalBorders - continuousFences) * totalElems)
  }

  val part1 = scores.map(_._1).sum
  val part2 = scores.map(_._2).sum

  println(part1)
  println(part2)
}




