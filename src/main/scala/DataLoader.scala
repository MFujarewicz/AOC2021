import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

object DataLoader {
  def apply(day: Int) = {
    Using(Source.fromResource(s"day$day.txt")) {
      source => source.getLines().toList
    }.get
  }
}
