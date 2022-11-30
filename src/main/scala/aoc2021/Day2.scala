package aoc2021

import toolbox.DataLoader

object Day2 extends App {
  val data = DataLoader(2)
  val xyTranslation = data.map {
    case s"forward $x" => (x.toInt, 0)
    case s"down $y" => (0, y.toInt)
    case s"up $y" => (0, -y.toInt)
  }

  val finalPosition = xyTranslation.reduce((a, b) => (a._1 + b._1, b._2 + a._2))
  println(finalPosition)
  println(finalPosition._2 * finalPosition._1)

  case class Position(horizontal: Int, depth: Int, aim: Int)

  val finalPositionWithAim = xyTranslation.foldLeft(Position(0, 0, 0))((pos, ins) => {
    Position(pos.horizontal + ins._1, pos.depth + pos.aim * ins._1, pos.aim + ins._2)
  })
  println(finalPositionWithAim)
  println(finalPositionWithAim.depth * finalPositionWithAim.horizontal)

}
