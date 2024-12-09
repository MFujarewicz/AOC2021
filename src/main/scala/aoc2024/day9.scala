package aoc2024

import toolbox.DataLoader

trait Block {
  def value: Int
}

case class File(value: Int, size: Int, var moved: Boolean = false) extends Block {
  override def toString: String = value.toString.head.toString
}

case object Empty extends Block {
  override def toString: String = "."

  def value = 0
}


@main def day9(): Unit = {
  val usingTestData = false
  val input = DataLoader(9, 2024, useTestData = usingTestData).head

  var disk = input.zipWithIndex.flatMap { case (size, index) =>
    if index % 2 != 0 then {
      List.fill(size.toString.toInt)(Empty)
    } else {
      List.fill(size.toString.toInt)(File(index / 2, size.toString.toInt))
    }
  }.toArray

//  println(disk.mkString(""))
  var l = 0
  var r = disk.indices.last

  while l <= r do {

    if disk(l) != Empty then {
      l += 1
    } else {

      if disk(r) == Empty then {
        r -= 1
      } else {

        val buf = disk(l)
        disk.update(l, disk(r))
        disk.update(r, buf)
      }

    }

  }


//  println(disk.mkString(""))

  if usingTestData then assert(disk.mkString("") == "0099811188827773336446555566..............")

  val res1 = disk.zipWithIndex.map { case (block, index) => index * block.value }.map(BigInt(_)).sum
  println(res1)

  //part2
  disk = input.zipWithIndex.flatMap { case (size, index) =>
    if index % 2 != 0 then {
      List.fill(size.toString.toInt)(Empty)
    } else {
      List.fill(size.toString.toInt)(File(index / 2, size.toString.toInt))
    }
  }.toArray


//  println(disk.map(_.value).max)
  println(disk.mkString(""))


  l = 0
  r = disk.indices.last

  while r > l do {

    if(disk(r) == Empty){
      r-=1
    }else{

      val block = disk(r) match
        case file:File if !file.moved => {

          val availableIndex = disk.mkString("").indexOf("."*file.size)//xd
//          println(availableIndex)
          if availableIndex != -1 && availableIndex < r-file.size +1  then {



            (availableIndex until availableIndex + file.size).foreach(i => disk.update(i, file.copy(moved = true)))
            (r - file.size + 1 to r).foreach(i => disk.update(i, Empty))
//            println(disk.mkString(""))

          }
          else{
            r-=file.size
          }
        }
        case _ => r-=1
    }
  }

//  println(disk.mkString(""))
  val res2 = disk.zipWithIndex.map { case (block, index) => index * block.value }.map(BigInt(_)).sum
  println(res2)


}
