package aoc2023

import toolbox.DataLoader


object day2 extends App {

  val data = DataLoader(2, 2023, false)

  println(data)


  sealed trait Color;

  case object Red extends Color;

  case object Green extends Color;

  case object Blue extends Color;


  val redCubes = 12;
  val greenCubes = 13;
  val blueCubes = 14;

  val cubeCounts = Map(
    Red -> redCubes,
    Green -> greenCubes,
    Blue -> blueCubes,
  )

  val colorSpellingMap: Map[String, Color] = Map(
    "red" -> Red,
    "green" -> Green,
    "blue" -> Blue
  )


  case class Hand(cubes: Map[Color, Int])
  case class Game(index: Int, hands: List[Hand]) {
    def maxRed = hands.map(_.cubes.getOrElse(Red, 0)).max
    def maxBlue = hands.map(_.cubes.getOrElse(Blue, 0)).max
    def maxGreen = hands.map(_.cubes.getOrElse(Green, 0)).max
  }

  val games = data.map {
    case s"Game $index: $cubesString" => {
      val hands: List[Hand] = cubesString.split("; ").map(s => {
        s.split(", ").map {
          case s"$count $color" => (colorSpellingMap(color) -> count.toInt)
        }.toMap
      }).map(Hand(_)).toList
      Game(index.toInt, hands)
    }
  }



  val result1 = games.filter(game =>
    game.hands.forall(hand => hand.cubes.forall{case (color, count) => count <= cubeCounts(color)})
  ).map(_.index).sum

  println(result1)


  val result2 = games.map( g =>
       g.maxRed * g.maxBlue * g.maxGreen
  ).sum

  println(result2)





}
