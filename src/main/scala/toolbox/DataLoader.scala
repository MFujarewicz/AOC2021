package toolbox

import scala.io.Source
import scala.util.Using

object DataLoader {
  def apply(day: Int, year: Int) = {
    Using(Source.fromResource(s"$year/day$day.txt")) {
      source => source.getLines().toList
    }.get
  }
}
