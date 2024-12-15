package aoc2024.day13.day13

import toolbox.DataLoader

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*


case class Button(x: Double, y: Double, cost: Double)

case class Pos(x: Double, y: Double)

case class ClawMachine(A_button: Button, B_button: Button, target: Pos) {
  def coinsToWin: Option[Double] = {

    val allCosts = for {
      aTimes <- 0 to 100
      bTimes <- 0 to 100
      if aTimes * A_button.x + bTimes * B_button.x == target.x && aTimes * A_button.y + bTimes * B_button.y == target.y
    }
    yield (
      aTimes * A_button.cost + bTimes * B_button.cost
      )

    if allCosts.isEmpty then None else Some(allCosts.min)
  }

  def coinsToWinWithMath: Option[Double] = {

    val Tx:Double = target.x
    val Ty:Double = target.y
    val Ax: Double = A_button.x
    val Ay: Double = A_button.y
    val Bx: Double = B_button.x
    val By: Double = B_button.y

    val B = (Ax*Ty-Ay*Tx)/(By*Ax-Bx*Ay)
    val A = (Tx-B*Bx)/Ax

    if (A%1 == 0 && B%1 == 0) {
      Some(A*A_button.cost+B*B_button.cost)
    } else {
      None
    }
  }
}

@main def day13(): Unit = {
  val usingTestData = false
  val clawMachines = DataLoader(13, 2024, useTestData = usingTestData).grouped(4).map(lines => {
    val a = lines(0) match {
      case s"Button A: X+$x, Y+$y" => Button(x.toInt, y.toInt, 3)
    }
    val b = lines(1) match {
      case s"Button B: X+$x, Y+$y" => Button(x.toInt, y.toInt, 1)
    }
    lines(2) match {
      case s"Prize: X=$x, Y=$y" => ClawMachine(a, b, Pos(x.toInt, y.toInt))
    }
  }).toList


  val part1 = clawMachines.flatMap(_.coinsToWinWithMath).sum.toLong
  println(part1)


  val part2Offset = 10000000000000D
  val part2machines = clawMachines.map(c => c.copy(target = Pos(c.target.x + part2Offset, c.target.y + part2Offset)))

  val part2 = part2machines.flatMap(_.coinsToWinWithMath).sum.toLong
  println(part2)



}




