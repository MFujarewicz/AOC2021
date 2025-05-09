package aoc2023

import toolbox.DataLoader

import scala.collection.parallel.CollectionConverters.*

object day7_2 extends App {

  val data = DataLoader(7, 2023, useTestData = false)


//  Five of a kind AAAAA -> 6
//  Four of a kind AA8AA -> 5
//  Full house 23332 -> 4
//  Three of a kind AAA23 -> 3
//  Two pair 23432 -> 2
//  One pair A23A4 -> 1
//  High card A23A4 -> 0


  val cardPower = List('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J').reverse.zipWithIndex.toMap

  case class Handbid(hand: String, bid: Int) {

    val c1 = hand(0)
    val c2 = hand(1)
    val c3 = hand(2)
    val c4 = hand(3)
    val c5 = hand(4)

    val jokerCount = hand.count(_ == 'J')

    val counts = hand.toList.groupBy(identity).view.mapValues(_.size).values.toList
    val countsNoJoker = hand.toList.filter(_ != 'J').groupBy(identity).view.mapValues(_.size).values.toList


    def strength: Int = {

      //Five of a kind
      if (counts.contains(5)) return 6
      if (countsNoJoker.max + jokerCount == 5) return 6

      //Four of a kind
      if (counts.contains(4)) return 5
      if (countsNoJoker.max + jokerCount == 4) return 5

      //Full house
      if (counts.contains(3) && counts.contains(2)) return 4
      if (countsNoJoker.sorted.reverse.take(2).sum + jokerCount == 5) return 4

      //Three of a kind
      if (counts.contains(3)) return 3
      if (countsNoJoker.max + jokerCount == 3) return 3

      //Two Pairs
      if (counts.count(_ == 2) == 2) return 2
      if (countsNoJoker.sorted.reverse.take(2).sum + jokerCount == 4) return 2

      //Pair
      if (counts.contains(2)) return 1
      if (jokerCount >= 1) return 1
      return 0
    }
  }

  def compareHands(h1: Handbid, h2: Handbid): Boolean = {

    if (h1.strength != h2.strength){
      h1.strength > h2.strength
    } else {

      val zipped = h1.hand.zip(h2.hand).dropWhile(t => t._1 == t._2)
      if (zipped.nonEmpty){
        val c1 = zipped.head._1
        val c2 = zipped.head._2

        cardPower(c1) > cardPower(c2)
      }
      else return false

    }

  }

  val handbids = data.map{case s"$hand $bid" => Handbid(hand, bid.toInt)}

//  handbids.foreach(println)

  val sorted = handbids.sortWith(compareHands).reverse.zipWithIndex.map{case (hb, s) => (hb, s+1)}


//  sorted.foreach(println)

  var result1 = sorted.map{case (hb, rank) => hb.bid*rank}.sum
  println(s"result 1 = $result1")


}
