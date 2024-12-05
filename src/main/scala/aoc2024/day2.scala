package aoc2024

import toolbox.DataLoader

@main def day2(): Unit = {
  val input = DataLoader(2, 2024, useTestData = false).map(_.split(" ").map(_.toInt).toList)

  def isReportSafe(report: List[Int]) = {

    val sorted = report.sorted

    val zipped = sorted.zip(sorted.drop(1))

    zipped.forall(t => t._2-t._1 >= 1) && zipped.forall(t => t._2-t._1 <= 3) && (report == sorted || report == sorted.reverse)
  }

  def isReportSafer(report: List[Int]):Boolean = {
    val size = report.size
    report.indices.map(
      i => report.take(i) ++ report.drop(i+1)
    ).exists(isReportSafe)
  }

  println(input.count(isReportSafe))
  println(input.count(isReportSafer))

//  input.foreach(a =>
//    println(s"$a ${isReportSafer(a)}")
//  )
}
