package aoc2022.day11_2

import toolbox.DataLoader

import scala.collection.mutable

val useTestData = false

var allDivisors: BigInt = 0

trait Operation {
  def calculateNewValue(oldValue: BigInt): BigInt
}

case object OldTimesOld extends Operation {
  override def calculateNewValue(oldValue: BigInt): BigInt = (oldValue * oldValue) % allDivisors
}

case object OldPlusOld extends Operation {
  override def calculateNewValue(oldValue: BigInt): BigInt = (oldValue + oldValue) % allDivisors
}

case class OldPlusNumber(addThis: BigInt) extends Operation {
  override def calculateNewValue(oldValue: BigInt): BigInt = (oldValue + addThis) % allDivisors
}

case class OldTimesNumber(multiplier: BigInt) extends Operation {
  override def calculateNewValue(oldValue: BigInt): BigInt = (oldValue * multiplier) % allDivisors
}

case class Monkey(number: BigInt, operation: Operation, testDivisor: BigInt, items: mutable.ListBuffer[BigInt], trueMonkey: BigInt, falseMonkey: BigInt) {

  var inspectionCounter: BigInt = 0

  def test(item: BigInt): Boolean = item % testDivisor == 0

  def receiveItem(item: BigInt): Unit = items.append(item)

  def throwStuff(trueMonkey: Monkey, falseMonkey: Monkey): Unit = {
    for (item <- items) {
      inspectionCounter+=1
      val newValue = operation.calculateNewValue(item)
      if test(newValue) then trueMonkey.receiveItem(newValue) else falseMonkey.receiveItem(newValue)
    }
    items.clear()
  }
}

class Monkeys(monkeys: List[Monkey]) {


  def monkeyBusiness():BigInt = {
    monkeys.map(_.inspectionCounter).sorted.reverse.take(2).product
  }

  val monkeyCount = monkeys.size

  def getMonkey(i: BigInt): Monkey = monkeys.find(_.number == i).get

  override def toString: String = {
    val stringBuilder = new mutable.StringBuilder()


    for (i <- 0 until monkeyCount) {
      val monkey = getMonkey(i)
      stringBuilder.append(
        s"  Monkey ${monkey.number}: ${monkey.items.mkString(", ")}\n"
      )
    }

    stringBuilder.toString()
  }

  def throwStuffAround(): Unit = {
    for {i <- 0 until monkeyCount} {
      val monkey = getMonkey(i)

      monkey.throwStuff(getMonkey(monkey.trueMonkey), getMonkey(monkey.falseMonkey))

    }
  }

}

@main def day11(): Unit = {

  val data = DataLoader(day = 11, year = 2022, useTestData = useTestData)

  val grouped = data.filter(!_.isEmpty).grouped(6)
  //  data.filter(!_.isEmpty).grouped(6).toList.head.zipWithIndex.foreach(l => println(s"${l._2}: \"${l._1}\""))

  println()
  val monkeysParsed = grouped.toList.map(list => {
    val monkeyNumber = list(0) match {
      case s"Monkey $number:" => number.toInt
    }
    val items = list(1) match {
      case s"  Starting items: $itemList" => itemList.split(" ").map(_.filter(_ != ',').toInt)
    }
    val operation = list(2) match {
      case s"  Operation: new = old * old" => OldTimesOld
      case s"  Operation: new = old + old" => OldPlusOld
      case s"  Operation: new = old + $number" => OldPlusNumber(number.toInt)
      case s"  Operation: new = old * $number" => OldTimesNumber(number.toInt)
    }
    val divisor = list(3) match {
      case s"  Test: divisible by $divisor" => divisor.toInt
    }
    val trueMonkey = list(4) match {
      case s"    If true: throw to monkey $trueMonkey" => trueMonkey.toInt
    }
    val falseMonkey = list(5) match {
      case s"    If false: throw to monkey $falseMonkey" => falseMonkey.toInt
    }

    Monkey(monkeyNumber, operation, divisor, mutable.ListBuffer.from(items.map(BigInt(_))), trueMonkey, falseMonkey)
  })

  val monkeys = Monkeys(monkeysParsed)

  allDivisors = monkeysParsed.map(_.testDivisor).product

  println(monkeysParsed.map(_.testDivisor))
  println(allDivisors)



  val maxRounds = 10000
  for (roundNumber <- 0 until maxRounds){
    println(s"after round $roundNumber: \n$monkeys \n")
    monkeys.throwStuffAround()
  }
  println(s"after round $maxRounds: \n$monkeys \n")

  println(monkeys.monkeyBusiness())


}
