package aoc2024

import aoc2024.Direction._
import toolbox.DataLoader

case class Pos(x: Int, y: Int){
  def move(direction: Direction): Pos = {
    direction match
      case Up => Pos(x-1, y)
      case Down => Pos(x+1, y)
      case Left => Pos(x, y-1)
      case Right => Pos(x, y+1)
  }
}

class Sim(val map: Vector[Vector[Field]], var guard: Pos, var direction: Direction){

  var stopSim = false
  var visited = 1
  var stopSimReason = StopSimReason.SimNotStopped

  enum StopSimReason {
    case OutOfBounds, Loop, SimNotStopped
  }

  private def turnRight(direction: Direction): Direction = {
    direction match
      case Up => Right
      case Down => Left
      case Left => Up
      case Right => Down
  }



  private def isInBounds(pos: Pos) = map.indices.contains(pos.x) && map.head.indices.contains(pos.y)

  private def step = {
    val newPos = guard.move(direction)

    if isInBounds(newPos) then {
      val newField = map(newPos.x)(newPos.y)

      if newField.isObstacle then {
        //place i want to go is obstacle
        direction = turnRight(direction)
//        println(direction)
      } else {
        guard = newPos
//        println(guard)

        if !newField.visited then visited += 1
        newField.visited = true
        if newField.pastDirections.contains(direction) then {
          stopSim = true
          stopSimReason = StopSimReason.Loop
        }
        newField.pastDirections = newField.pastDirections + direction
      }
    }else{
      stopSim = true
      stopSimReason = StopSimReason.OutOfBounds
    }
  }

  def runSim: Unit = {
    while (!stopSim) {
      step
    }
  }
}

enum Direction:
  case Up, Down, Left, Right



class Field(var visited: Boolean, var isObstacle: Boolean, var pastDirections: Set[Direction] = Set.empty)

@main def day6(): Unit = {
  val input = DataLoader(6, 2024, useTestData = false)

  def newMapFromInput = {
    input.map(_.toVector.map {
      case '#' => Field(visited = false, isObstacle = true)
      case '.' => Field(visited = false, isObstacle = false)
      case '^' => Field(visited = true, isObstacle = false)
      case _ => throw new Exception("bad character in input")
    }).toVector
  }

  val map = newMapFromInput


  val startPos = (for {
    x <- map.indices
    y <- map(x).indices if map(x)(y).visited
  } yield (
    Pos(x, y)
    )).head
//  println(startPos)

  val sim = Sim(map, startPos, Up)

  sim.runSim
  println(sim.visited)

  //part 2


  val answer2 = (for {
    x <- map.indices
    y <- map(x).indices if !map(x)(y).isObstacle
  } yield {
    val map = newMapFromInput

    map(x)(y).isObstacle = true
    val sim = Sim(map, startPos, Up)
    sim.runSim

    if sim.stopSimReason == sim.StopSimReason.Loop then 1 else 0
  }).sum

  println(answer2)
}
