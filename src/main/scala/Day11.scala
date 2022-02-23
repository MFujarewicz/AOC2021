object Day11 extends App {
  val data = DataLoader(11)

  val octopuses = Octopuses(data)
  var flashCounter = 0
  var haveAllFlashed = false

  for i <- 1 to 1000 do {
    octopuses.nextStep
    if octopuses.haveAllFlashed && !haveAllFlashed then {
      haveAllFlashed = true
      println(s"first time octopuses flashed all at once step: ${i}")
    }
    if i == 100 then println(s"total flashes at step 100: $flashCounter")
  }



  case class Octopus(counter: Int, flashed: Boolean){
    override def toString: String = counter.toString
    def increaseCounter: Octopus = Octopus(counter+1, flashed)
    def setFlashed: Octopus = {
      flashCounter += 1
      Octopus(counter, true)
    }
    def setNotFlashed: Octopus = Octopus(counter, false)
    def zeroCounter: Octopus = Octopus(0, flashed)
  }


  class Octopuses(state: Array[Array[Octopus]]){

    override def toString: String = {
      state.map(_.mkString(" ")).mkString("\n")
    }

    var haveAllFlashed = false

    def nextStep = {
      setAllNotFlashed
      increaseAllByOne
      flash
      allFlashCheck
      setFlashedToZero
      this
    }

    def allFlashCheck = {
      val allOctopuses = for x <- state.head.indices; y <- state.indices yield state(x)(y)
      haveAllFlashed = allOctopuses.forall(_.flashed)
    }



    def increaseAllByOne = {
      for x <- state.head.indices; y <- state.indices do {
        state(x)(y) = state(x)(y).increaseCounter
      }
    }

    def setAllNotFlashed = {
      for x <- state.head.indices; y <- state.indices do {
        state(x)(y) = state(x)(y).setNotFlashed
      }
    }

    def setFlashedToZero = {
      for x <- state.head.indices; y <- state.indices do {
        if state(x)(y).flashed then state(x)(y) = state(x)(y).zeroCounter
      }
    }

    def containsUnflashed = {
      state.flatten.exists(o => o.counter>9 && !o.flashed)
    }

    private def flash: Unit = {
      while containsUnflashed do {
        for x <- state.head.indices; y <- state.indices do {
          val octopus  = state(x)(y)
          if octopus.counter > 9 && !octopus.flashed then {
            //set current status to flashed
            state(x)(y) = octopus.setFlashed
            //set neighours couters + 1
            List((x+1, y), (x-1, y), (x, y+1), (x, y-1), (x+1, y+1), (x+1, y-1), (x-1, y+1), (x-1, y-1))
              .filter((x, y) => state.isDefinedAt(x) && state(x).isDefinedAt(y))
              .foreach((x, y) => state(x)(y) = state(x)(y).increaseCounter)
          }
        }
      }
    }
  }
  object Octopuses{
    def apply(data: List[String]): Octopuses = {
      val array = data.map(_.split("").map(value => Octopus(value.toInt, false))).toArray
      new Octopuses(array)
    }
  }
}
