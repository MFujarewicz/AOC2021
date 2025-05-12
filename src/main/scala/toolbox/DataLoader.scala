package toolbox

import scala.io.Source
import scala.util.Using

object GreatestCommonDivisor {
  def apply(a: Long, b: Long): Long = {
    val A = math.max(a, b)
    val B = math.min(a, b)

    if A == B then A else GreatestCommonDivisor(A - B, B)
  }
}

object LeastCommonMultiple {
  def apply(a: Long, b: Long): Long = {

    a * (b / GreatestCommonDivisor(a, b))
  }
}

object DataLoader {
  def apply(day: Int, year: Int, useTestData: Boolean = false): List[String] = {

    val path = if useTestData then s"$year/day${day}_test.txt" else s"$year/day$day.txt"

    Using(Source.fromResource(path)) {
      source => source.getLines().toList
    }.get
  }
}
