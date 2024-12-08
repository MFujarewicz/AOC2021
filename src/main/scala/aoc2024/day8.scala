package aoc2024

import toolbox.DataLoader

case class Tower(x: Int, y: Int, freq: Char)
case class Resonance(x: Int, y: Int)

def findResonances(t1: Tower, t2: Tower) ={
  val xDist = math.abs(t1.x - t2.x)
  val yDist = math.abs(t1.y - t2.y)

  val maxX = math.max(t1.x, t2.x)
  val maxY = math.max(t1.y, t2.y)
  val minX = math.min(t1.x, t2.x)
  val minY = math.min(t1.y, t2.y)

  val y1 = if (t1.y == minY){
    minY - yDist
  }else{
    maxY + yDist
  }

  val y2 = if (t2.y == minY) {
    minY - yDist
  } else {
    maxY + yDist
  }

  List(
    Resonance(math.min(maxX + xDist, minX - xDist), y1),
    Resonance(math.max(maxX + xDist, minX - xDist), y2),
  )
}

def findResonances2(t1: Tower, t2: Tower): List[Resonance] = {
  val xDiff = t1.x - t2.x
  val yDiff = t1.y - t2.y


  (-55 to 55).map(i =>
    Resonance(t1.x+xDiff*i, t1.y+yDiff*i)
  ).toList
}

def showMap(resonances: List[Resonance], towers: List[Tower], input: List[String]) = {

  for (x <- input.indices){
    for (y <- input.head.indices){

      (x, y) match
        case (x, y) if towers.exists(t => t.x == x && t.y == y) => print(towers.find(t => t.x == x && t.y == y).get.freq)
        case (x, y) if resonances.exists(t => t.x == x && t.y == y) => print('#')
        case _ => print('.')

    }
    println()
  }


}

@main def day8(): Unit = {
  val input = DataLoader(8, 2024, useTestData = false)

  val towers = input.zipWithIndex.flatMap {case (line, x) => {
      line.zipWithIndex.flatMap {case (freq, y) => {
        if freq != '.' then List(Tower(x, y, freq)) else Nil
      }}
    }
  }
  println(towers)

  val resonances = towers.groupBy(_.freq).flatMap{case (freq, towerList) => {
   val allPairs = towerList.permutations.map(_.take(2)).toList.map(_.toSet).distinct
    println(allPairs)

    allPairs.flatMap(p => {
      val t1 = p.toList(0)
      val t2 = p.toList(1)

      findResonances(t1, t2)

    })
  }}.filter(res => {
    input.indices.contains(res.x) && input.head.toList.indices.contains(res.y)
  })
  println(resonances)


  val resonances2 = towers.groupBy(_.freq).flatMap { case (freq, towerList) => {
    val allPairs = towerList.permutations.map(_.take(2)).toList.map(_.toSet).distinct
    println(allPairs)

    allPairs.flatMap(p => {
      val t1 = p.toList(0)
      val t2 = p.toList(1)

      findResonances2(t1, t2)

    })
  }
  }.filter(res => {
    input.indices.contains(res.x) && input.head.toList.indices.contains(res.y)
  })

  println(resonances.toSet.size)
  println(resonances2.toSet.size)


  showMap(resonances2.toList, towers, input)



}
