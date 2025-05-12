package aoc2023

import toolbox.{DataLoader, LeastCommonMultiple}

import scala.collection.parallel.CollectionConverters.*

object day8_2 extends App {

  val data = DataLoader(8, 2023, useTestData = false)

  val steps = data.head

  case class Node(self: String, left: String, right: String)

  val nodes = data.drop(2).map { case s"$self = ($left, $right)" => Node(self, left, right) }

  val nodeMap = nodes.map(n => (n.self, n)).toMap

  var startNodes = nodes.filter(_.self.endsWith("A"))

  def countSteps(start: Node): Int = {
    var currentNode = start
    var stepCounter = 0

    while (!currentNode.self.endsWith("Z")) {

      currentNode = steps((stepCounter % steps.length).toInt) match {
        case 'L' => nodeMap(currentNode.left)
        case 'R' => nodeMap(currentNode.right)
      }

      stepCounter += 1
    }

    stepCounter
  }

  val stepCounts = nodes.filter(_.self.endsWith("A")).map(countSteps)

  val result = stepCounts.map(_.toLong).reduce(LeastCommonMultiple.apply)

  println(result)


}
