package aoc2024

import toolbox.DataLoader

case class Rule(left: Int, right: Int) {
  private def isApplicableToUpdate(pages: Vector[Int]): Boolean = {
    pages.contains(left) && pages.contains(right)
  }
  def satisfiesRule(pages: Vector[Int]): Boolean = {
    !isApplicableToUpdate(pages) ||
      pages.indexOf(left) < pages.indexOf(right)
  }
}

@main def day4(): Unit = {
  val input = DataLoader(4, 2024, useTestData = false)

  val rules = input.filter(_.contains("|")).map{
    case s"$left|$right" => Rule(left.toInt, right.toInt)
  }

  val updates: List[Vector[Int]] = input.dropWhile(_.nonEmpty).drop(1).map(_.split(",").map(_.toInt).toVector)

  def isCorrect(pages: Vector[Int]): Boolean = {
    rules.forall(_.satisfiesRule(pages))
  }

  def ruleCompare(a: Int, b: Int): Boolean = {
    val rule = rules.find(r => {
      (r.right == a && r.left == b) || (r.right == b && r.left == b)
    })
    rule.forall(_.left == a)
  }

  val answer1 = updates.filter(isCorrect).map(v => v(v.size/2)).sum
  val answer2 = updates.filterNot(isCorrect).map(_.sortWith(ruleCompare)).map(v => v(v.size/2)).sum

  println(answer1)
  println(answer2)
}
