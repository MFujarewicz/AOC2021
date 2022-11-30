package aoc2021

import toolbox.DataLoader

object Day3 extends App {
  val data: List[String] = DataLoader(3, 2021)

  val characterCount = data.length

  val a = data.map(_.toList.map(_.toString)).reduce((a, b) => {
    a.zipWithIndex.map(x => x._1 + b(x._2))
  }).map(s => s.count(_.equals('1')))

  val gamma = a.map(i => if i > characterCount / 2 then 1 else 0)
  val epsilon = gamma.map(x => (x + 1) % 2)

  def toDecimal(list: List[Int]): Int = {
    list.reverse.zipWithIndex.map(a => a._1 * math.pow(2, a._2).toInt).sum
  }

  def toDecimal(list: String): Int = {
    toDecimal(list.map(c => c.toString.toInt).toList)
  }

  println(toDecimal(gamma) * toDecimal(epsilon))

  //part 2
  def filterMostCommon(data: List[String]) = {
    val firstBitOccurrencesCount = data.map(_.head).groupMapReduce(identity)(_ => 1)(_ + _).withDefault(_ => 0)
    val mostCommonFirstBit = if firstBitOccurrencesCount.getOrElse('0', 0) > firstBitOccurrencesCount('1') then '0' else '1'
    data.filter(_.head == mostCommonFirstBit)
  }

  def filterLeastCommon(data: List[String]) = {
    val firstBitOccurrencesCount = data.map(_.head).groupMapReduce(identity)(_ => 1)(_ + _).withDefault(_ => 0)
    val leastCommonFirstBit = if firstBitOccurrencesCount('1') == firstBitOccurrencesCount('0') then '0' else if firstBitOccurrencesCount('0') < firstBitOccurrencesCount('1') then '0' else '1'
    data.filter(_.head == leastCommonFirstBit)
  }

  def findMostCommonRec(data: List[String]): String = {
    val filtered = filterMostCommon(data)
    if filtered.distinct.length == 1 then filtered.head else filtered.head.head + findMostCommonRec(filtered.map(_.tail))
  }

  def findLeastCommonRec(data: List[String]): String = {
    val filtered = filterLeastCommon(data)
    if filtered.distinct.length == 1 then filtered.head else filtered.head.head + findLeastCommonRec(filtered.map(_.tail))
  }

  val v1 = findMostCommonRec(data)
  val v2 = findLeastCommonRec(data)
}
