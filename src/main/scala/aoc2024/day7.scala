package aoc2024

import aoc2024.Operator._
import toolbox.DataLoader

case class Calibration(result: BigInt, numbers: List[Int]) {
  def canBeTrue: Boolean = {
    val allOprs = generatePossibleOperators(numbers.size-1)


    allOprs.map(oprs => {
      oprs.zipWithIndex.foldLeft(BigInt(numbers.head))((acc, op) => {
        op._1 match
          case Operator.Mul => acc * numbers(op._2 + 1)
          case Operator.Add => acc + numbers(op._2 + 1)
          case Operator.Con => BigInt(acc.toString + numbers(op._2 + 1).toString)
      })
    }).contains(result)
  }
}

enum Operator:
  case Mul, Add, Con

def generatePossibleOperators(i: Int) = {

  def genRec(i: Int, oprs: List[List[Operator]]): List[List[Operator]] = {
    if (i == 0){
      oprs
    } else {
      val newOprs = oprs.flatMap(operList => {
        operList.appended(Mul) :: operList.appended(Add) :: operList.appended(Con) :: Nil
      })
      genRec(i-1, newOprs)
    }
  }
  genRec(i, List(List.empty))

}


@main def day7(): Unit = {
  val input = DataLoader(7, 2024, useTestData = false)

  val data = input.map{
    case s"$result: $numbers" => Calibration(BigInt(result), numbers.split(" ").map(_.toInt).toList)
  }

  println(generatePossibleOperators(3))



  println(data.filter(_.canBeTrue).map(_.result).sum)
}
