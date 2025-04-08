package aoc2023

import toolbox.DataLoader


object day3 extends App {

  val data = DataLoader(3, 2023, useTestData = false).toVector.map(_.toVector)

  //  data.foreach(println)

  def isSymbolAround(x: Int, y: Int): Boolean = {
    var symbolFound = false

    for (xOffset <- List(-1, 0, 1);
         yOffset <- List(-1, 0, 1)
         if (xOffset != 0 || yOffset != 0) && !symbolFound
         ) {

      val char = data.applyOrElse(y + yOffset, _ => List.empty).applyOrElse(x + xOffset, _ => '.')

      if (char != '.' && !char.isDigit) {
        symbolFound = true
      }

    }
    symbolFound

  }

  def getStarAround(x: Int, y: Int): Set[(Int, Int)] = {
    var symbolFound = false
    var coords = Set.empty[(Int, Int)]

    for (xOffset <- List(-1, 0, 1);
         yOffset <- List(-1, 0, 1)
         if (xOffset != 0 || yOffset != 0)
         ) {

      val char = data.applyOrElse(y + yOffset, _ => List.empty).applyOrElse(x + xOffset, _ => '.')

      if (char == '*') {
        coords = coords ++ Set(((x + xOffset), (y + yOffset)))
      }

    }
    coords

  }

  var currentNumber = 0
  var symbolFound = false
  var allPartSum = 0
  var stars = Set[(Int, Int)]()
  val starMap = scala.collection.mutable.Map[(Int, Int), List[Int]]()


  for (y <- data.indices) {

    for (x <- data.head.indices) {

      if (data(y)(x).isDigit) {
        currentNumber = currentNumber * 10 + data(y)(x).toString.toInt
        if (isSymbolAround(x, y)) symbolFound = true
        stars = stars ++ getStarAround(x, y)
      } else {
        if (symbolFound) allPartSum += currentNumber
        symbolFound = false


        stars.foreach{case (x, y) => {
          val currentList = if (starMap.isDefinedAt((x, y))) starMap((x, y)) else List.empty
          starMap.addOne((x, y) -> (currentList :+ currentNumber))
        }}
        currentNumber = 0
        stars = Set.empty
      }


    }
    if (symbolFound) allPartSum += currentNumber
    symbolFound = false
    stars.foreach { case (x, y) => {
      val currentList = if (starMap.isDefinedAt((x, y))) starMap((x, y)) else List.empty
      starMap.addOne((x, y) -> (currentList :+ currentNumber))
    }
    }
    currentNumber = 0
    stars = Set.empty
  }

  println(allPartSum)
  println(starMap)

  val result2 = starMap.filter(_._2.size == 2).values.map(_.product).sum
  println(result2)


}
