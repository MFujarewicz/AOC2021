package aoc2024

import toolbox.DataLoader

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._


@main def day11(): Unit = {
  val usingTestData = false
  var stonesInput = DataLoader(11, 2024, useTestData = usingTestData).head.split(" ").map(BigInt(_)).toList.par

  var stones = stonesInput


  for (i <- 1 to 25){
//    println(s"iter: $i size: ${stones.size} stones: $stones")
//    println(i)
    stones = stones.par.flatMap{i =>
//      println(i)
      i match
        case _ if i == 0 => List(1)
        case _ if i.toString.length % 2 == 0 => {
          val str = i.toString
          List(str.take(str.length/2), str.drop(str.length/2)).map(s => BigInt(s))
        }
        case _ if i.toString.length % 2 != 0 => List(i*2024)
        case _ => throw new Exception("wtf")
        }
    }



  println(stones.size)


  //part 2
  var stoneMap = mutable.Map[BigInt, BigInt]()
  stonesInput.foreach(s =>
    stoneMap.updateWith(s)(o => Some(o.getOrElse(BigInt(0))+1))
  )


  for (i <- 1 to 75){
//    println(stoneMap.size)
//    println(stoneMap)
    val stoneMapOld = stoneMap.toList
    stoneMap = mutable.Map[BigInt, BigInt]()

    stoneMapOld.foreach{case (stone, count) =>
      val newStones: List[BigInt] = stone match
        case _ if stone == 0 => List(1)
        case _ if stone.toString.length % 2 == 0 => {
          val str = stone.toString
          List(str.take(str.length / 2), str.drop(str.length / 2)).map(s => BigInt(s))
        }
        case _ if stone.toString.length % 2 != 0 => List(stone * 2024)
        case _ => throw new Exception("wtf")
//      println(newStones)

      newStones.map(s => stoneMap.updateWith(s)(o => Some(o.getOrElse(BigInt(0))+count)))
    }
  }


  println(stoneMap.values.sum)


}

