object Day6 extends App{
  val fishes = DataLoader(6).flatMap(_.split(',')).map(s => s.toInt)

  println(simulateLanternfish(fishes, 80).size)
  println(simulateLanternfishSmart(fishes, 256))

  def simulateLanternfish(fishes: List[Int], generations: Int): List[Int] = {
    assert(generations >= 0)
    generations match {
      case 0 => fishes
      case _ => simulateLanternfish(fishes.flatMap(x => if x > 0 then List(x-1) else List(6, 8)), generations-1)
    }
  }

  def simulateLanternfishSmart(fishes:  List[Int], generations: Int): Long = {
    val fishMap = fishes.groupMapReduce(identity)(_ => 1l)(_+_).withDefault(_ => 0l)

    def simulateLanternFishRec(fishMap: Map[Int, Long], generations: Int): Long = {
      generations match {
        case 0 => fishMap.values.sum
        case _ => {
          val newFishMap = Map(
            0 -> fishMap(1),
            1 -> fishMap(2),
            2 -> fishMap(3),
            3 -> fishMap(4),
            4 -> fishMap(5),
            5 -> fishMap(6),
            6 -> (fishMap(7)+fishMap(0)),
            7 -> fishMap(8),
            8 -> fishMap(0),
          )
          simulateLanternFishRec(newFishMap, generations-1)
        }
      }
    }
    simulateLanternFishRec(fishMap, generations)
  }


}
