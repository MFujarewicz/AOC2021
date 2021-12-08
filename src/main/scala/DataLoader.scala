import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

object DataLoader {
  def apply(day: Int) = {
    Using(Source.fromFile(s"src/main/resources/day$day.txt")) {
      source => source.getLines().toList
    }.get
  }
}
