object Day13 extends App {
  val data = DataLoader(13)

  case class Point(x: Int, y: Int)
  trait Axis
  case object X extends Axis
  case object Y extends Axis
  case class Fold(axis: Axis, coordinate: Int)

  val points = data.flatMap({
    case s"$x,$y" => Some(Point(x.toInt, y.toInt))
    case _ => None
  })

  val folds = data.flatMap({
    case s"fold along $axis=$coordinate" => Some((axis, coordinate.toInt))
    case _ => None
  }).map({
    case ("x", coordinate: Int) => Fold(X, coordinate)
    case ("y", coordinate: Int) => Fold(Y, coordinate)
  })


  def foldPaper(paper: List[Point], fold: Fold): List[Point] ={
    paper.map(point => fold match {
      case Fold(X, coordinates) => Point(coordinates - math.abs(point.x-coordinates), point.y)
      case Fold(Y, coordinates) => Point(point.x, coordinates - math.abs(point.y-coordinates))
    }).distinct
  }
  val afterFold1 = foldPaper(points, folds.head)
  println(afterFold1.size)

  val afterAllFolds = folds.foldLeft(points)(foldPaper)//:)

  def showPaper(paper: List[Point]): Unit = {
    val MAX_X = paper.map(_.x).max
    val MAX_Y = paper.map(_.y).max

    val paperSet = paper.toSet

    for y <- 0 to MAX_Y do
      for x <- 0 to MAX_X do
        if paperSet.contains(Point(x, y)) then print("#") else print(" ")
      end for
      println()
    end for
  }

  showPaper(afterAllFolds)
}
