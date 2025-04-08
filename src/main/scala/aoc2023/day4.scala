package aoc2023

import toolbox.DataLoader


object day4 extends App {

  val data = DataLoader(4, 2023, useTestData = false)

  //  data.foreach(println)


  case class Card(index: Int, winning: List[Int], numbers: List[Int], var count: Int = 1) {
    def score: Int = {
      val power = numbers.count(n => winning.contains(n)) - 1
      math.pow(2, power).toInt
    }

    def cardsWon: Int = {
      numbers.count(n => winning.contains(n)) - 1
    }
  }

  val cards = data.zipWithIndex.map(s => {
      val numbersStr = s._1.dropWhile(_ != ':').drop(2)
    //    println(numbersStr)
    val winning = numbersStr.split('|').head.split(' ').filter(_.nonEmpty).map(_.toInt).toList
    val numbers = numbersStr.split('|')(1).split(' ').filter(_.nonEmpty).map(_.toInt).toList

    Card(s._2 + 1, winning, numbers)
  }
  )

  println(cards.map(_.score).sum)

  for (card <- cards;
       wonCardIndex <- card.index to card.index + card.cardsWon) {
    cards(wonCardIndex).count += card.count
  }

  println(cards.map(_.count).sum)
}
