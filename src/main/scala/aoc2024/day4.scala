package aoc2024

import toolbox.DataLoader

@main def day4(): Unit = {
  val input = DataLoader(4, 2024, useTestData = false).map(_.toVector).toVector

  /*left to right*/
  def readLetter(xy: XY) = {
    val x = xy.x
    val y = xy.y
    input.applyOrElse(x, _ => Vector.empty).applyOrElse(y, _ => ' ')
  }

  case class XY(x: Int, y: Int) {
    def R = XY(x + 1, y)
    def L = XY(x - 1, y)
    def D = XY(x, y + 1)
    def U = XY(x, y - 1)

    def RD = this.R.D
    def RU = this.R.U
    def LD = this.L.D
    def LU = this.L.U

    def all8dir = List(
      _4L,
      _4R,
      _4D,
      _4U,
      _4RD,
      _4RU,
      _4LD,
      _4LU,
    )

    def XMAS = {
      List(
        LU,
        RU,
        this,
        LD,
        RD
      )
    }

    def _4R = {
      List(this, this.R, this.R.R, this.R.R.R)
    }

    def _4L = {
      List(this, this.L, this.L.L, this.L.L.L)
    }

    def _4D = {
      List(this, this.D, this.D.D, this.D.D.D)
    }

    def _4U = {
      List(this, this.U, this.U.U, this.U.U.U)
    }

    def _4RD = {
      List(this, this.RD, this.RD.RD, this.RD.RD.RD)
    }

    def _4RU = {
      List(this, this.RU, this.RU.RU, this.RU.RU.RU)
    }

    def _4LD = {
      List(this, this.LD, this.LD.LD, this.LD.LD.LD)
    }

    def _4LU = {
      List(this, this.LU, this.LU.LU, this.LU.LU.LU)
    }
  }

  def readAll(xy: XY) = {
    xy.all8dir.map(_.map(xy => readLetter(xy)).filter(_ != ' ').mkString(""))
  }

  def readXMAS(xy: XY) = {
    xy.XMAS.map(xy => readLetter(xy)).filter(_ != ' ').mkString("")
  }


  var counter = 0
  var counter2 = 0

  for {
    x <- input.indices
    y <- input(x).indices
  } yield {
    val xy = XY(x, y)
    readAll(xy).filter(_ == "XMAS").foreach(_ => counter = counter + 1)

    val xmas = readXMAS(xy)
    if(xmas.applyOrElse(2, _ => ' ') == 'A'){
      println(xmas)
    }

    xmas match
      case "MMASS" => counter2+=1
      case "SSAMM" => counter2+=1
      case "MSAMS" => counter2+=1
      case "SMASM" => counter2+=1

      case _ => ()
  }


//  println(readAll(XY(0,0)))
  println(counter)
  println(counter2)


}
