package aoc2024

import toolbox.DataLoader

@main def day1(): Unit = {
  val input = DataLoader(1, 2024)
    .map{
      case s"$listA   $listB" => (listA.toInt, listB.toInt)
    }
  val listA = input.map(_._1).sorted
  val listB = input.map(_._2).sorted

  val distances = listA.zip(listB).map{case (a, b) => math.abs(a-b)}

  println(distances.sum)

  val part2 = listA.map(a => {
    listB.count(_ == a) * a
  }).sum

  println(part2)
}
