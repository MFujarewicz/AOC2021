package aoc2021

import toolbox.DataLoader

object Day7 extends App {
  val data = DataLoader(7)
  val positions = data.head.split(',').map(_.toInt)

  val targetPositions = (0 to 1000).toList

  val fuelExpenses = targetPositions.map(targetPosition => {
    positions.map(p => math.abs(p - targetPosition)).sum
  })

  val fuelExpenses2 = targetPositions.map(targetPosition => {
    positions.map(p => math.abs(p - targetPosition)).map(x => x * (x + 1) / 2).sum
  })


  println(fuelExpenses.min)
  println(fuelExpenses2.min)
}
