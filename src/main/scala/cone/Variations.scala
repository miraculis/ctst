package cone

import scala.io.Source

case class Rec(ul:Int, ur:Int, dl:Int, dr:Int) {
  override def toString: String = {
    ul.toString + " " + ur.toString + " " + dl.toString + " " + dr.toString
  }
}

object Rec {
  def fromLine(line: String) : Rec = {
    val nums: Array[Int] = line.split(" ").map(_.toInt)
    new Rec(nums(0), nums(1), nums(2), nums(3))
  }
}

object Variations {
  type P = Array[Rec]

  def main(a:Array[String]) = {
    val data = read(a(0))
    permutate(data, data.length, printOut)
  }

  def permutate(data: P, n: Int, c : P => Unit): Unit = {
    if (check(data, n)) {
      if (n == 1) {
        c(data)
      } else {
        for (i <- n - 1 to 0 by -1) {
          swap(data, i, n - 1)
          permutate(data, n - 1, c)
          swap(data, i, n - 1)
        }
      }
    }
  }

  def swap(d:P, i:Int, j:Int): Unit = {
    val swap = d(j)
    d(j) = d(i)
    d(i) = swap
  }

  def check(xs:P, n :Int) : Boolean = {
    n match {
      case 10 => check10(xs)
      case 8 => check8(xs)
      case 7 => check7(xs)
      case 6 => check6(xs)
      case 5 => check5(xs)
      case 4 => check4(xs)
      case 3 => check3(xs)
      case 2 => check2(xs)
      case 1 => check1(xs)
      case _ => true
    }
  }

  def check10(xs:P):Boolean = {
    xs(10).dr + xs(11).dl <= 10
  }

  def check8(xs:P):Boolean = {
    xs(8).dr + xs(9).dl + xs(11).ur <= 10
  }

  def check7(xs:P):Boolean = {
    xs(7).dr + xs(8).dl + xs(10).ur + xs(11).ul == 10
  }

  def check6(xs:P):Boolean = {
    xs(6).dr + xs(7).dl + xs(10).ul <= 10
  }

  def check5(xs:P):Boolean = {
    xs(5).dr + xs(9).ur <= 10
  }

  def check4(xs:P):Boolean = {
    xs(4).dr + xs(5).dl + xs(8).ur + xs(9).ul == 10
  }

  def check3(xs:P):Boolean = {
    xs(3).dr + xs(4).dl + xs(7).ur + xs(8).ul == 10
  }

  def check2(xs:P):Boolean = {
    xs(2).dr + xs(3).dl + xs(6).ur + xs(7).ul == 10 &&
      xs(2).dl + xs(6).ul <= 10
  }

  def check1(xs:P):Boolean = {
    xs(0).dl + xs(2).ur + xs(3).ul <= 10 &&
    xs(0).ur + xs(1).ul <= 10 &&
    xs(0).dr + xs(1).dl + xs(3).ur + xs(4).ul == 10 &&
    xs(1).dr + xs(4).ur + xs(5).ul <= 10
  }

  def read(filename:String) : Array[Rec] = {
    Source.fromFile(filename).getLines.collect{case line: String => Rec.fromLine(line)}.toArray
  }

  def printOut(x:P) = {
    x.foreach(println(_))
    println()
  }
}
