package aoc2022.day8

import toolbox.DataLoader
import scala.collection.mutable.ListBuffer


@main def day8(): Unit = {
  val trees = DataLoader(8, 2022, useTestData = false)
    .map(_.map(_.toString.toInt).toVector).toVector

  val maxX = trees.indices.max
  val maxY = trees.head.indices.max

  def isVisible(x: Int, y: Int): Boolean = {
    val height = trees(x)(y)

    val line1 = (for {
      i <- 0 until x
    } yield trees(i)(y))

    val line2 = (for {
      i <- x + 1 to maxX
    } yield trees(i)(y))

    val line3 = (for {
      j <- 0 until y
    } yield trees(x)(j))

    val line4 = (for {
      j <- y + 1 to maxY
    } yield trees(x)(j))

    val max1 = line1.maxOption.getOrElse(-1)
    val max2 = line2.maxOption.getOrElse(-1)
    val max3 = line3.maxOption.getOrElse(-1)
    val max4 = line4.maxOption.getOrElse(-1)

    height > max1
      || height > max2
      || height > max3
      || height > max4
  }

  def scenicScore(x: Int, y: Int): Int = {
    val height = trees(x)(y)


    //up
    val line1 = (for {
      i <- 0 until x
    } yield trees(i)(y))
      .reverse

    //down
    val line2 = (for {
      i <- x + 1 to maxX
    } yield trees(i)(y))

    //left
    val line3 = (for {
      j <- 0 until y
    } yield trees(x)(j))
      .reverse

    //right
    val line4 = (for {
      j <- y + 1 to maxY
    } yield trees(x)(j))

    val visibleCounts = List(line1, line2, line3, line4).map(line => {
      val visible = line.takeWhile(_ < height)
      visible.length + (if visible.length < line.length then 1 else 0)
    })

    visibleCounts.product
  }

  val visibleTrees = for {
    y <- trees.head.indices
    x <- trees.indices
    if isVisible(x, y)}
  yield (x, y)

  println(visibleTrees.size)

  val scenicScores = for {
    y <- trees.head.indices
    x <- trees.indices}
  yield scenicScore(x, y)

  println(scenicScores.max)

}
