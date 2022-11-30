package aoc2021

import toolbox.DataLoader

import scala.annotation.tailrec
import scala.collection.mutable

object Day10 extends App :

  val input = DataLoader(10, 2021).map(stringToBracketList)

  val answer1 = input.flatMap(findCorrupted).map(_.corruptScore).sum
  println(answer1)

  val scores = input.filter(l => findCorrupted(l).isEmpty).map(findMissing).map(calculateCompleteScore).sorted
  val answer2 = scores(scores.length / 2)
  println(answer2)

  def calculateCompleteScore(input: List[Bracket]): Long = {
    input.map(_.completeScore).reverse.zipWithIndex.map((score, index) => score * math.pow(5, index)).sum.toLong
  }

  def findCorrupted(input: List[Bracket]): Option[Bracket] = {
    @tailrec
    def findCorruptedRec(input: List[Bracket], stack: mutable.Stack[Bracket]): Option[Bracket] = {
      input match {
        case Nil => None
        case (x: Opening) :: xs => findCorruptedRec(xs, stack.push(x))
        case (x: Closing) :: xs if stack.head == x.counterpart => findCorruptedRec(xs, stack.tail)
        case x :: xs => Some(x)
      }
    }

    findCorruptedRec(input, mutable.Stack[Bracket]())
  }

  def findMissing(input: List[Bracket]): List[Bracket] = {
    @tailrec
    def findMissingRec(input: List[Bracket], stack: mutable.Stack[Bracket]): List[Bracket] = {
      input match {
        case Nil => stack.toList
        case (x: Opening) :: xs => findMissingRec(xs, stack.push(x))
        case (x: Closing) :: xs => findMissingRec(xs, stack.tail)
      }
    }

    findMissingRec(input, mutable.Stack[Bracket]())
  }


  trait Bracket {
    def counterpart: Bracket

    def corruptScore: Int

    def completeScore: Int
  }

  trait Opening extends Bracket

  trait Closing extends Bracket {
    override def corruptScore: Int = counterpart.corruptScore

    override def completeScore: Int = counterpart.completeScore
  }

  case object bracket1open extends Opening {
    override def counterpart: Bracket = bracket1close

    override def corruptScore: Int = 3

    override def completeScore: Int = 1

    override def toString: String = "("
  }

  case object bracket1close extends Closing {
    override def counterpart: Bracket = bracket1open

    override def toString: String = ")"
  }

  case object bracket2open extends Opening {
    override def counterpart: Bracket = bracket1close

    override def corruptScore: Int = 57

    override def completeScore: Int = 2

    override def toString: String = "["
  }

  case object bracket2close extends Closing {
    override def counterpart: Bracket = bracket2open

    override def toString: String = "]"
  }

  case object bracket3open extends Opening {
    override def counterpart: Bracket = bracket1close

    override def corruptScore: Int = 1197

    override def completeScore: Int = 3

    override def toString: String = "{"
  }

  case object bracket3close extends Closing {
    override def counterpart: Bracket = bracket3open

    override def toString: String = "}"
  }

  case object bracket4open extends Opening {
    override def counterpart: Bracket = bracket4close

    override def corruptScore: Int = 25137

    override def completeScore: Int = 4

    override def toString: String = "<"
  }

  case object bracket4close extends Closing {
    override def counterpart: Bracket = bracket4open

    override def toString: String = ">"
  }

  def stringToBracketList(s: String): List[Bracket] = {
    s.map(charToBracket).toList
  }

  def charToBracket(c: Char): Bracket = {
    c match {
      case '(' => bracket1open
      case ')' => bracket1close
      case '[' => bracket2open
      case ']' => bracket2close
      case '{' => bracket3open
      case '}' => bracket3close
      case '<' => bracket4open
      case '>' => bracket4close
    }
  }
