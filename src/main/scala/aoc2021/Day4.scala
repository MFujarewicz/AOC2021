package aoc2021

import toolbox.DataLoader

import scala.annotation.targetName
import scala.io.Source.{fromFile, fromResource}

object Day4 extends App {
  val data: List[String] = DataLoader(4)

  val bingoNumbers = data(0).split(",").map(_.toInt).toList

  val allBoards = data.tail.sliding(6, 6)
    .map(_.tail)
    .map(_.flatMap(_.split(" ")))
    .map(_.map(_.replaceAll(" +", " ")))
    .map(_.filter(_.nonEmpty))
    .map(_.map(_.toInt))
    .map(_.sliding(5, 5).toList)
    .map(Board(_))

  val scores = allBoards.map(_.play(bingoNumbers)).toList

  val wonGame = scores.minBy(_._1)
  val lastGame = scores.maxBy(_._1)

  println(wonGame)
  println(lastGame)


}

case class Board(numbers: List[List[Int]]){
  val rows = numbers
  val columns = (0 to 4).toList.map(i => numbers.map(_(i)))
  def play(list: List[Int]) = {

    def play(list: List[Int], round: Int):(Int, Int) = {
      val playingNumbers = list.take(round)
      val crossedRows = rows.map(_.filter(playingNumbers.contains(_)))
      val crossedColumns = columns.map(_.filter(playingNumbers.contains(_)))
      val isWon = crossedRows.exists(_.length == 5) || crossedColumns.exists(_.length == 5)

      if isWon then {
        val score = (numbers.map(_.sum).sum - crossedRows.map(_.sum).sum)*list(round-1)
        (round, score)
      }
      else
        play(list, round+1)
    }
    play(list, 5)
  }
}