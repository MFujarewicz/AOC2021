package aoc2022
import toolbox.DataLoader

@main def day5(): Unit = {

    val data: List[String] = DataLoader(5, 2022, useTestData = false)

  case class Stacks(stack: List[List[Char]]) {

    val topCrates = stack.map(_.headOption.getOrElse("")).mkString

    def move(instruction: MoveInstruction): Stacks = {
      val newStack = stack.indices.map(i => i match
        case instruction.from => stack(i).drop(instruction.amount)
        case instruction.to => stack(instruction.from).take(instruction.amount).reverse ++ stack(i)
        case _ => stack(i)
      )
      Stacks(newStack.toList)
    }

    def moveAll(instruction: MoveInstruction): Stacks = {
      val newStack = stack.indices.map(i => i match
        case instruction.from => stack(i).drop(instruction.amount)
        case instruction.to => stack(instruction.from).take(instruction.amount) ++ stack(i)
        case _ => stack(i)
      )
      Stacks(newStack.toList)
    }
  }

  case class MoveInstruction(amount: Int, from: Int, to: Int)

  object MoveInstruction{
    def fromString(s: String) = { s match
      case s"move $amount from $from to $to" => MoveInstruction(amount.toInt, from.toInt-1, to.toInt-1)
    }
  }

  val startData = data.filter(!_.contains("move")).filter(_.nonEmpty).filter(!_.startsWith(" 1"))
  val movesData = data.filter(_.contains("move"))
  val stackNumbers = data.filter(_.startsWith(" 1"))

  val height = startData.size
  val width = stackNumbers.head.filter(!_.isWhitespace).last.toString.toInt

  val stacks = for {x <- 0 until width}
    yield for {y <- 0 until height }
      yield startData(y)(x * 4 + 1)


  val startingStacks = Stacks(stacks.toList.map(_.toList.filter(!_.isWhitespace)))

  val instructions = movesData.map(MoveInstruction.fromString)

  println(startingStacks)

  val finalStacks = instructions.foldLeft(startingStacks)((stacks, instruction) => stacks.move(instruction))
  val finalStacks2 = instructions.foldLeft(startingStacks)((stacks, instruction) => stacks.moveAll(instruction))

  println(finalStacks.topCrates)
  println(finalStacks2.topCrates)
}
