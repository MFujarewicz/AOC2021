package aoc2022.day6

import scala.util.control.Breaks._
import toolbox.DataLoader

@main def day6(): Unit = {
  val data = DataLoader(6, 2022, useTestData = false)
  val signal = data.head



  var i = 4
  var c1: Char = signal(0)
  var c2: Char = signal(1)
  var c3: Char = signal(2)
  var c4: Char = signal(3)
  //  println(s"$c1, $c2, $c3, $c4")

  if (
    c1 != c2 && c1 != c3 && c1 != c4
      && c2 != c3 && c2 != c4
      && c3 != c4
  ) println(s"Found start-of-packet at $i")

  breakable {
    for (c0 <- signal.drop(4)) {
      i += 1
      c1 = c2
      c2 = c3
      c3 = c4
      c4 = c0

      if (
        c1 != c2 && c1 != c3 && c1 != c4
          && c2 != c3 && c2 != c4
          && c3 != c4) {
        println(s"Found start-of-packet at $i")
        break
      }
    }
  }

  //inefficient but easier to write ¯\_(ツ)_/¯
  val windowSize = 14
  val part2result = signal.sliding(windowSize, 1)
    .map(s => s.groupMapReduce(identity)(_ => 1)(_+_))
    .map(_.values.max)
    .zipWithIndex
    .filter(t => t._1 == 1)
    .take(1)

  println(part2result.toList.head._2+windowSize)



}
