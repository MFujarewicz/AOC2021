import scala.annotation.{tailrec, targetName}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day10_2 extends App:
  val lines = DataLoader(10)

  val characterScore = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val characterPairs = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>',
  )

  val reversePairs = characterPairs.map((a, b) => (b, a))


  lines.map(line =>
    line.scanLeft(Map.empty[Char, Int].withDefault(_ => 0))((counts, char) => char match {
      case c:Char if characterPairs.isDefinedAt(c) => counts.updated(c, counts(c)+1)
      case c:Char => counts.updated(reversePairs(c), counts(reversePairs(c))-1)
    })
  ).foreach(println)

