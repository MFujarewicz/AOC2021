package aoc2021

import toolbox.DataLoader

object Day15 extends App {

  val data = DataLoader(15, 2021)

  class Graph(val riskVector: Vector[Vector[Int]]) {

    def graphMultipliedBy5: Graph = {
      val newRV = riskVector.map(line => (0 until 5).flatMap(i => line.map(_ + i)))
      val moreNewerRV = (0 until 5).flatMap(i => newRV.map(_.map(_ + i).toVector)).toVector
        .map(_.map(i => if i > 9 then i - 9 else i))

      new Graph(moreNewerRV)
    }

    type Vertex = (Int, Int)

    def getRisk(v: Vertex): Int = riskVector(v._2)(v._1)

    def getNeighbours(v: Vertex): Set[Vertex] = Set(
      (v._1 - 1, v._2),
      (v._1 + 1, v._2),
      (v._1, v._2 + 1),
      (v._1, v._2 - 1),
    ).filter(isInGraph)

    def isInGraph(v: Vertex): Boolean = {
      val xOK = v._1 >= 0 && v._1 < shape._1
      val yOK = v._2 >= 0 && v._2 < shape._2
      xOK && yOK
    }

    override def toString: String = riskVector.mkString("\n")

    def shape: Vertex = (riskVector.head.length, riskVector.length)

    def distance(start: Vertex, end: Vertex): Int = {
      val distances: Array[Array[Int]] = Array.fill(shape._1, shape._2)(Int.MaxValue)

      def updateDistance(vertex: Vertex, newValue: Int): Unit = {
        distances(vertex._2)(vertex._1) = newValue
      }

      def getDistance(vertex: Vertex): Int = distances(vertex._2)(vertex._1)

      updateDistance(start, 0)


      //Ordering.by(getDistance) <- terrible, terrible idea
      //works only because elements are inserted into priority queue after updating distance
      val allVertexes = scala.collection.mutable.PriorityQueue()(Ordering.by(getDistance).reverse)
      allVertexes.addOne(start)

      while (allVertexes.nonEmpty) {
        val v = allVertexes.dequeue()
        getNeighbours(v).foreach(n => {
          val newDistance = getDistance(v) + getRisk(n)
          getDistance(n) match
            case d if d > newDistance => {
              updateDistance(n, newDistance)
              allVertexes.addOne(n)
            }
            case _ => ()
        })
      }

      getDistance(end)
    }
  }


  object Graph {
    def apply(data: List[String]): Graph = {
      //all lines same length
      assert(data.forall(line => line.length == data.head.length), "all lines in input data must have same length")

      new Graph(data.map(_.map(_.toInt - 48).toVector).toVector)
    }
  }

  val graph = Graph(data)


  val result = graph.distance((0, 0), (graph.shape._1 - 1, graph.shape._2 - 1))
  println(result)

  val graph2 = graph.graphMultipliedBy5
  val result2 = graph2.distance((0, 0), (graph2.shape._1 - 1, graph2.shape._2 - 1))
  println(result2)


}
