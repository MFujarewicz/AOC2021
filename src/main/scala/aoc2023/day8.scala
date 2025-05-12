package aoc2023

import toolbox.DataLoader

import scala.collection.parallel.CollectionConverters.*

object day8 extends App {

  val data = DataLoader(8, 2023, useTestData = false)

  val steps = data.head

  case class Node(self: String, left: String, right: String)

  val nodes = data.drop(2).map{case s"$self = ($left, $right)" => Node(self, left, right)}

  println(steps)
  nodes.foreach(println)

  val nodeMap = nodes.map(n => (n.self, n)).toMap

  println(nodeMap)


  var currentNode = nodeMap("AAA")
  var stepCounter = 0

  while (currentNode.self != "ZZZ"){

    currentNode =  steps(stepCounter%steps.length) match {
      case 'L' => nodeMap(currentNode.left)
      case 'R' => nodeMap(currentNode.right)
    }

    stepCounter += 1
  }

  println(s"totalsteps: $stepCounter")




}
