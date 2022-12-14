package aoc2022.day1

import toolbox.DataLoader

@main def day1(): Unit = {
  val calorieSums = DataLoader(1, 2022).map(s =>  if s.isEmpty then "X" else s"V$s").reduce(_+_).split("X").map(s => s.split("V").filter(_.nonEmpty).map(_.toInt).sum)
  println(calorieSums.max)

  val top3 = calorieSums.sorted.reverse.take(3).sum
  println(top3)
}
