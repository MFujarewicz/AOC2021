package aoc2023

import toolbox.DataLoader


object day1 extends App {

  val data = DataLoader(1, 2023, false)

  println(data)

//  val result = data.map(
//    s => {
//      val numbers = s.filter(_.isDigit)
//      val n1 = numbers.head.toString.toInt
//      val n2 = numbers.last.toString.toInt
//      n2+10*n1
//    }
//  ).sum

  val spelledDigits = List(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
    "1" -> 1,
    "2" -> 2,
    "3" -> 3,
    "4" -> 4,
    "5" -> 5,
    "6" -> 6,
    "7" -> 7,
    "8" -> 8,
    "9" -> 9,
  )

  val res = data.map(str => {

    var minIndex = Int.MaxValue
    var minInt = 0

    var maxIndex = Int.MinValue
    var maxInt = 0

    for (kv <- spelledDigits) {
      val k = kv._1
      val v = kv._2

      val idx = str.indexOf(k)
      if (idx != -1 && idx < minIndex){
        minIndex = idx
        minInt = v
      }

      val lastIdx = str.lastIndexOf(k)
      if (lastIdx != -1 && lastIdx > maxIndex){
        maxIndex = lastIdx
        maxInt = v
      }

    }

    (str, minInt*10+maxInt)
  })

  println(res)
  println(res.map(_._2).sum)


}
