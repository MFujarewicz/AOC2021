import scala.io.Source

object Day1 extends App{

  def countIncrease(data: List[Int]) =
    data.foldLeft((Int.MaxValue, 0))((acc, depth) => if depth > acc._1 then (depth, acc._2+1) else (depth, acc._2))._2

  val data =  DataLoader(1).map(_.toInt)

  println(countIncrease(data))
  println(countIncrease(data.sliding(3).map(_.sum).toList))
}
