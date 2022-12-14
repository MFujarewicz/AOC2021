package aoc2022.day10

import toolbox.DataLoader


@main def day10(): Unit = {

  val instructions = DataLoader(day = 10, year = 2022, useTestData = false)


  var cycle = 1
  var registerX = 1
  val signals = {
    for (instruction <- instructions) yield {
      instruction match {
        case s"addx $x" => {
          val l = List((cycle, registerX), (cycle + 1, registerX))

          cycle += 2
          registerX += x.toInt
          l
        }
        case "noop" => {
          val l = List((cycle, registerX))
          cycle += 1
          l
        }
      }
    }
  }


  val flat = signals.flatten

  val strengths = 0 +: flat.map(t => t._1 * t._2)

  val result = List(
    strengths(20),
    strengths(60),
    strengths(100),
    strengths(140),
    strengths(180),
    strengths(220),
  ).sum

  println(result)

  //part 2
  for (i <- flat) {
    val cycle = i._1
    val spritePosition = i._2
    val  rayPos =   (cycle % 40) - 1

    if rayPos <= spritePosition + 1 && rayPos >= spritePosition - 1 then print("#") else print(".")


    if cycle % 40 == 0 then println
  }

}
