package aoc2022.day3

import toolbox.DataLoader

def itemPriority(c: Char): Int = {
  if c.isLower then c.toInt - 96 else c.toInt - 38
}

@main def day3(): Unit = {

  val data: List[String] = DataLoader(3, 2022, useTestData = false)

  val result = data
    .map(s => s.splitAt(s.length / 2))
    .map(t => t._1.toSet.intersect(t._2.toSet))
    .map(_.map(itemPriority).sum)
    .sum

  val priorityList = for {
    group <- data.sliding(3, 3)
    groupCommonItem <- group.map(_.toSet).reduce((s1, s2) => s1.intersect(s2))
  }
  yield itemPriority(groupCommonItem)

  val result2 = priorityList.sum

  println(result)
  println(result2)
}
