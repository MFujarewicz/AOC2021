package aoc2022.day2

import toolbox.DataLoader

val figureScoreMap = Map('X' -> 1, 'Y' -> 2, 'Z' -> 3)
val matchScoreMap = Map(
  "A X" -> 3, //rock  rock
  "A Y" -> 6, //rock paper
  "A Z" -> 0, //rock scissors

  "B X" -> 0, //paper  rock
  "B Y" -> 3, //paper paper
  "B Z" -> 6, //paper scissors

  "C X" -> 6, //scissors  rock
  "C Y" -> 0, //scissors paper
  "C Z" -> 3, //scissors scissors
)

val instructionDecipher = Map(
  "A X" -> "A Z", //rock scissors
  "A Y" -> "A X", //rock rock
  "A Z" -> "A Y", //rock paper

  "B X" -> "B X", //paper rock
  "B Y" -> "B Y", //paper paper
  "B Z" -> "B Z", //paper scissors

  "C X" -> "C Y", //scissors paper
  "C Y" -> "C Z", //scissors scissors
  "C Z" -> "C X", //scissors rock
)

@main def day2(): Unit = {
  val data: List[String] = DataLoader(2, 2022)

  println(data)

  val totalScore: Int = data.map(s =>
    figureScoreMap(s(2)) + matchScoreMap(s)
  ).sum

  println(totalScore)

  val totalScore2 = data.map(instructionDecipher)
    .map(s => figureScoreMap(s(2)) + matchScoreMap(s))
    .sum
  println(totalScore2)

  println(data.map(instructionDecipher))

}
