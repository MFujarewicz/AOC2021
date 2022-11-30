package aoc2021

import toolbox.DataLoader

object Day8 extends App {
  val data = DataLoader(8, 2021)

  val sizeMap = Map(
    2 -> 1,
    3 -> 7,
    4 -> 4,
    7 -> 8
  )
  val knownSizesSet = sizeMap.keys.toSet

  val secondInputSegments = data.map(_.split('|')(1).trim)
  val countKnownLengths = secondInputSegments.flatMap(_.split(" ")).map(_.length).count(length => knownSizesSet.contains(length))
  println(countKnownLengths)

  def outputNumber(input: String): Int = {
    var numberMap: Map[Int, String] = Map()
    val signals = input.split('|').head.trim.split(' ').toList.map(_.sorted)
    val display = input.split('|')(1).trim.split(' ').toList.map(_.sorted)


    numberMap += 1 -> signals.filter(_.length == 2).head

    numberMap += 4 -> signals.filter(_.length == 4).head

    numberMap += 7 -> signals.filter(_.length == 3).head
    numberMap += 8 -> signals.filter(_.length == 7).head

    numberMap += 3 -> signals.filter(_.length == 5).filter(s => s.contains(numberMap(1)(0)) && s.contains(numberMap(1)(1))).head

    numberMap += 9 -> signals.filter(_.length == 6)
      .filter(s => numberMap(1).forall(c => s.contains(c)))
      .filter(s => numberMap(3).forall(c => s.contains(c))).head

    numberMap += 0 -> signals.filter(_.length == 6)
      .filter(s => numberMap(1).forall(c => s.contains(c)))
      .filter(_ != numberMap(9)).head

    numberMap += 6 -> signals.filter(_.length == 6).filter(_ != numberMap(9)).filter(_ != numberMap(0)).head

    numberMap += 5 -> signals.filter(_.length == 5).filter(s => s.forall(c => numberMap(9).contains(c))).filter(_ != numberMap(3)).head

    numberMap += 2 -> signals.filter(_.length == 5).filter(_ != numberMap(5)).filter(_ != numberMap(3)).head

    val reverseNumberMap: Map[String, Int] = numberMap.map((a, b) => (b, a))

    display.map(reverseNumberMap(_)).reverse.zipWithIndex.map((a, i) => a * math.pow(10, i)).map(_.toInt).sum

  }


  println(data.map(outputNumber).sum)

}
