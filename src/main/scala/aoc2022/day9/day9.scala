package aoc2022.day9

import toolbox.DataLoader

import scala.annotation.targetName

case class Coordinates(x: Int, y: Int) {

  @targetName("add")
  def +(coordinates: Coordinates): Coordinates = {
    Coordinates(x+coordinates.x, y+coordinates.y)
  }

  @targetName("subtract")
  def -(coordinates: Coordinates): Coordinates = {
    Coordinates(x - coordinates.x, y - coordinates.y)
  }

  def isAdjacent(other: Coordinates): Boolean = {
    other.x >= x - 1 && other.x <= x + 1 &&
      other.y >= y - 1 && other.y <= y + 1

  }
}

object Coordinates {
  def zero: Coordinates = Coordinates(0, 0)
}

trait Direction {
  val translation: Coordinates
}
case object Up extends Direction {
  override val translation: Coordinates = Coordinates(0, 1)
}
case object Down extends Direction {
  override val translation: Coordinates = Coordinates(0, -1)
}
case object Left extends Direction {
  override val translation: Coordinates = Coordinates(-1, 0)
}
case object Right extends Direction {
  override val translation: Coordinates = Coordinates(1, 0)
}

case class Instruction(direction: Direction, step: Int)

object Instruction {
  def fromString(s: String): Instruction = {
    s match
      case s"U $step" => Instruction(Up, step.toInt)
      case s"D $step" => Instruction(Down, step.toInt)
      case s"L $step" => Instruction(Left, step.toInt)
      case s"R $step" => Instruction(Right, step.toInt)
  }
}

case class Knot(myPosition: Coordinates, path: List[Coordinates], child: Option[Knot]) {

  def getAllCoordinates: List[Coordinates] = {
    myPosition +: child.map(_.getAllCoordinates).getOrElse(List())
  }

  def move(direction: Direction): Knot = {
    val myNewPosition = myPosition + direction.translation
    val newChildren = child.map(_.moveAfterDad(myNewPosition))
    Knot(myNewPosition, path :+ myNewPosition, newChildren)
  }

  def calculateNewPosition(dadTo: Coordinates): Coordinates = {

    val diff = dadTo - myPosition

    val translationX = diff.x match {
      case 0 => 0
      case x if x > 0 => 1
      case x if x < 0 => -1
    }

    val translationY = diff.y match {
      case 0 => 0
      case y if y > 0 => 1
      case y if y < 0 => -1
    }

    Coordinates(myPosition.x + translationX, myPosition.y + translationY)
  }

  def moveAfterDad(dadTo: Coordinates): Knot = {
    if dadTo.isAdjacent(myPosition) then this else {
      val myNewPosition = calculateNewPosition(dadTo)
      val newChild = child.map(_.moveAfterDad(myNewPosition))
      Knot(myNewPosition, path :+ myNewPosition, newChild)
    }
  }

  def getTail: Knot = {
    child match
      case Some(child) => child.getTail
      case None => this
  }

  def visitedFieldsCount(): Int = path.toSet.size
}

object Knot {
  def initialize(length: Int): Knot = {
    def initializeImpl(length: Int): Option[Knot] = {
      if length <= 0 then None else Some(Knot(Coordinates.zero, List(Coordinates.zero), initializeImpl(length - 1)))
    }

    initializeImpl(length).get
  }
}

@main def day9(): Unit = {

  val directions = DataLoader(day = 9, year = 2022, useTestData = false)
    .map(Instruction.fromString)
    .flatMap(instruction => (0 until instruction.step).map(_ => instruction.direction))

  val rope1 = Knot.initialize(2)
  val rope2 = Knot.initialize(10)

  val endRope1 = directions.foldLeft(rope1)((rope, direction) => rope.move(direction))
  val endRope2 = directions.foldLeft(rope2)((rope, direction) => rope.move(direction))

  println(endRope1.getTail.visitedFieldsCount())
  println(endRope2.getTail.visitedFieldsCount())
}
