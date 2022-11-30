package aoc2021

import toolbox.DataLoader

object Day14 extends App {

  val data = DataLoader(14, 2021)
  val recipe = data.drop(2).map({ case s"$pair -> $insertion" => pair -> insertion }).toMap
  val seed = data.head.sliding(2).toList.groupMapReduce(identity)(_ => 1l)(_ + _)

  println(polymerize(seed, recipe, iterations = 10))
  println(polymerize(seed, recipe, iterations = 40))

  def polymerize(seed: Map[String, Long], recipe: Map[String, String], iterations: Int) = {

    def insertionStep(pairs: Map[String, Long], recipe: Map[String, String]) = {
      pairs.toList.flatMap((pair, count) => List(s"${pair.head}${recipe(pair)}" -> count, s"${recipe(pair)}${pair.tail}" -> count))
        .groupMapReduce(_._1)(_._2)(_ + _)
    }

    val counts = (1 to iterations).foldLeft(seed)((seed, _) => insertionStep(seed, recipe))
      .toList.flatMap((pair, count) => List(pair.head -> count, pair.tail -> count))
      .groupMapReduce(_._1)(_._2)(_ + _).map((a, b) => a.toString -> b)
      .map(_._2)
    counts.max - counts.min
  }
}
