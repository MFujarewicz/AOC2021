package aoc2023

import toolbox.DataLoader

import scala.collection.parallel.CollectionConverters._

object day5 extends App {

  val data = DataLoader(5, 2023, useTestData = false)

  case class /*Almanac*/ EntryRow(destinationStart: Long, sourceStart: Long, range: Long) {
    def containsSource(s: Long) = s >= sourceStart && s < sourceStart + range

    def translate(s: Long) = s - sourceStart + destinationStart
  }

  case class Entry(rows: List[EntryRow]) {
    def translate(s: Long) = {
      val customRange = rows.dropWhile(!_.containsSource(s))
      if (customRange.nonEmpty) {
        customRange.head.translate(s)
      }
      else {
        s
      }
    }
  }

  case class Almanac(entries: List[Entry]) {
    def translate(s: Long): Long = {
      entries.foldLeft(s){case (acc, entry) => entry.translate(acc)}
    }
  }



  def extractMap(title: String) =
    Entry(data.dropWhile(!_.contains(title)).drop(1).takeWhile(_.nonEmpty).map(s => {
      val split = s.split(" ").map(_.toLong)
      EntryRow(split(0), split(1), split(2))
    }))

  val seeds = data.head.dropWhile(!_.isDigit).split(" ").map(_.toLong).toList
  val transformSteps = data.drop(1).filter(_.contains(':'))
  val maps = transformSteps.map(extractMap)

  val almanac = Almanac(maps)

  println(seeds.map(s => almanac.translate(s)).min)


//  val seeds2 = seeds.grouped(2).map(l => l(0) to l(0) + l(1)).map(_.toList).reduce(_ ++ _)
//
//  println(s"total seeds to check: ${seeds2.size}")
//  println(seeds2.map(s => almanac.translate(s)).min)

  val seeds2 = seeds.grouped(2).map(l => l(0) until l(0) + l(1)).toList

  println(seeds2)

  val totalToCheck = seeds2.map(_.size).sum
  var counter = 0L;

  var min = Long.MaxValue;
  for (seedRange <- seeds2.zipWithIndex; seed <- seedRange._1){

    val location = almanac.translate(seed)

    if (location < min) min = location
    counter += 1;


    if (counter%10000000 == 0){
      println(counter.toDouble/totalToCheck)
    }
  }

  println(s"min: $min")


}
