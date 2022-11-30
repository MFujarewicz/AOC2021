package aoc2021

import toolbox.DataLoader

object Day12 extends App {
  val data = DataLoader(12)

  val allConnections = data.flatMap(s => {
    val pair = s.split("-")
    val a = pair(0)
    val b = pair(1)
    List((a, b), (b, a))
  }).groupMap(_._1)(_._2)

  val allPaths = generatePaths(allConnections, "start", "end")

  println(allPaths.length)

  def generatePaths(connections: Map[String, List[String]], start: String, end: String): List[List[String]] = {

    def containsTwoVisitsToSmallCave(path: List[String]): Boolean = {
      path.filter(s => s == s.toLowerCase).groupMapReduce(s => s)(_ => 1)(_ + _).values.exists(_ > 1)
    }

    def generatePathsRec(prefixes: List[List[String]]): List[List[String]] = {
      val finishedPaths = prefixes.filter(_.last == end)
      val unfinishedPats = prefixes.filter(_.last != end)

      val longerPaths = unfinishedPats.flatMap(prefix => {
        val possibleNext = connections(prefix.last)
          .filter(_ != start)
          .filter(s => s != s.toLowerCase || !prefix.contains(s)
            || !containsTwoVisitsToSmallCave(prefix) // comment this line to answer question 1
          )

        possibleNext.map(s => prefix :+ s)
      })
      val newPrefixes = finishedPaths ++ longerPaths
      if newPrefixes == prefixes then prefixes else generatePathsRec(newPrefixes)
    }

    generatePathsRec(List(List(start)))
  }

}
