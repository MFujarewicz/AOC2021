package aoc2023

import toolbox.DataLoader

import scala.collection.parallel.CollectionConverters.*

object day6 extends App {

  val data = DataLoader(6, 2023, useTestData = false)

  println(generateDistances(7))


  data.foreach(println)

  val times = data.head.split(" ").filter(_.nonEmpty).tail.map(_.toInt).toList
  println(times)

  val distances = data.tail.head.split(" ").filter(_.nonEmpty).tail.map(_.toInt).toList
  println(distances)


  val result = times.indices.map(i => {
    val time = times(i)
    val distance = distances(i)

    val possibleDistances = generateDistances(time)

    possibleDistances.filter(d => d > distance).size


  }).product

  println(result)


  def generateDistances(time: Long) = {
    (0L to time).map(timePressed => timePressed*(time-timePressed))
  }

  //part 2

  val part2time = times.map(_.toString).mkString("").toLong
  val part2distance = distances.map(_.toString).mkString("").toLong

  println(part2time)
  println(part2distance)

  val result2 = generateDistances(part2time).filter(d => d > part2distance).size
  println(result2)

}
