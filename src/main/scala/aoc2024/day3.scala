package aoc2024

import toolbox.DataLoader

@main def day3(): Unit = {
  var input = DataLoader(3, 2024, useTestData = false).mkString("")

  println(input)
//  println(input.size)

  var sum = 0
  var enableMul = 1

  while (input.nonEmpty){
    val candidateLastIndex = input.indexOf(')')
    val candidate = input.take(candidateLastIndex+1)
//    println(candidate)

    candidate match
      case s"mul($a,$b)" if a.size <= 3 && b.size <= 3 && a.forall(_.isDigit) && b.forall(_.isDigit ) =>
        sum += a.toInt*b.toInt*enableMul
      case "do()" => enableMul = 1
      case "don't()" => enableMul = 0
      case _ => ()

    input = input.drop(1)
  }

  println(sum)



}
