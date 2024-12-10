package aoc2024

import toolbox.DataLoader

import scala.collection.mutable


@main def day10(): Unit = {
  val usingTestData = false
  val map = DataLoader(10, 2024, useTestData = usingTestData).toVector.map(_.toVector.map(_.toString.toInt))

  val pathValues: mutable.Map[(Int, Int), Set[(Int, Int)]] = mutable.Map.empty
  val part2: mutable.Map[(Int, Int), Int] = mutable.Map.empty


  for {i <- List(9)
       x <- map.indices
       y <- map(x).indices
       if map(x)(y) == i
       } {

    pathValues.update((x, y), List((x, y)).toSet)
    part2.update((x, y), 1)
  }


  for {i <- 8 to 0 by -1
       x <- map.indices
       y <- map(x).indices
       if map(x)(y) == i
       } {

    List((1, 0), (-1, 0), (0, 1), (0, -1)).foreach(offset =>
      if map.applyOrElse(x+offset._1, _ => Nil).applyOrElse(y+offset._2, _ => -1) == i+1 then {
        pathValues.update((x, y), pathValues.getOrElse((x, y), Set.empty) ++ pathValues.getOrElse((x+offset._1, y+offset._2), Set.empty))
        part2.update((x, y), part2.getOrElse((x, y), 0) + part2.getOrElse((x+offset._1, y+offset._2), 0))
      }
    )
  }


  val res1 = pathValues.filter { case (x, y) -> value =>
    map(x)(y) == 0}
    .map(a =>
      a
    )
    .values
    .map(s => s.size)
    .sum
  println(res1)

  val res2 = part2.filter { case (x, y) -> value =>
      map(x)(y) == 0
    }
    .values
    .sum
  println(res2)
}

