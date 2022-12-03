package toolbox

import scala.io.Source
import scala.util.Using

object DataLoader {
  def apply(day: Int, year: Int, useTestData: Boolean = false): List[String] = {

    val path = if useTestData then s"$year/day${day}_test.txt" else s"$year/day$day.txt"

    Using(Source.fromResource(path)) {
      source => source.getLines().toList
    }.get
  }
}
