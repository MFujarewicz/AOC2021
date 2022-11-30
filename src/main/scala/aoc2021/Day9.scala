package aoc2021

import toolbox.DataLoader

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day9 extends App:
  val heightmap = Heightmap(DataLoader(9, 2021))
  println(heightmap.riskLevelSum)
  val biggest3basinsMultiplied = heightmap.basinSizes.toList.sorted.reverse.take(3).product
  println(biggest3basinsMultiplied)


class Heightmap(map: Vector[Vector[Int]]):
  override def toString: String = map.map(_.mkString(" ")).mkString("\n")

  type Point = (Int, Int)

  extension (point: Point)
    def height: Int = map(point._1)(point._2)

    def neighbours = neighboursImplementation(point)

    def lowerNeighbour = if point.isLowest then point else
      point.neighbours.filter(neighbour => neighbour.height < point.height).head

    def isLowest = point.neighbours.forall(neighbour => neighbour.height > point.height)

    @tailrec
    final def basinPoint: Point = if point.isLowest then point else point.lowerNeighbour.basinPoint


  private val allPoints: Seq[Point] =
    for x <- map.head.indices; y <- map.indices yield (x, y)

  private val MAX_X = map.head.indices.last
  private val MAX_Y = map.indices.last

  private def neighboursImplementation(point: Point): List[Point] =

    val x = point._1
    val y = point._2
    List(
      if x == 0 then (x, y) else (x-1, y),
      if x == MAX_Y then (x, y) else (x+1, y),
      if y == 0 then (x, y) else (x, y-1),
      if y == MAX_Y then (x, y) else (x, y+1),
    ).filter(neighbour => neighbour != point)

  lazy val lowPointsCoordinates: Set[Point] =
    val lowPoints = for point <- allPoints
      if point.isLowest
      yield point
    lowPoints.toSet

  lazy val riskLevelSum: Int = lowPointsCoordinates.toSeq.map(p => p.height+1).sum

  lazy val basinMap: Map[Point, Point] =

    val basins = for point <- allPoints
      if point.height != 9
      yield point -> basinPoint(point)

    basins.toMap

  lazy val basinSizes: Iterable[Int] =
    basinMap.groupMapReduce(xy=> xy._2)(_ => 1)(_+_).values

object Heightmap:
  def apply(data: List[String]): Heightmap =
    val map = data.map(s => s.map(_.toString.toInt))
    new Heightmap(map.map(_.toVector).toVector)


