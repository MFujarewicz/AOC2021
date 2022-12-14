package aoc2022.day0

import toolbox.DataLoader

trait conventionalMorality {
  def isGood: Boolean
  def isEvil = !isGood
}
case class Good(name: String) extends conventionalMorality {
  def isGood = true
}
case class Evil(name: String) extends conventionalMorality {
  def isGood = false
}

object day0 extends App {
  val data = DataLoader(0, 2022)


  val parsed:List[conventionalMorality] = data.map {
    case s"$name -> good" => Good(name)
    case s"$name -> evil" => Evil(name)
  }


  val goodCount = parsed.count(_.isGood)
  val evilCount = parsed.count(_.isEvil)

  println(s"good: $goodCount")
  println(s"evil: $evilCount")

}