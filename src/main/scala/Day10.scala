import scala.annotation.{tailrec, targetName}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day10 extends App:
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

  assert(characterScore.keys.toSet == characterPairs.values.toSet)
  val expectedCharacters = characterPairs.values.toSet ++ characterPairs.keys.toSet
  assert(lines.forall(s => s.forall(expectedCharacters.contains)))

  println(lines)

  def firstIncorrectCharacter(line: String): Option[Char] = {

    def firstIncorrectCharacterRec(line: String, counts: Map[Char, Int]): Option[Char] = {
      line match {
        case "" => None
        case s:String => ???

      }
    }
    val freshCount = characterPairs.values.map(c => (c, 0)).toMap
    firstIncorrectCharacterRec(line, freshCount)
  }

