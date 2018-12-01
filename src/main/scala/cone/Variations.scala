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
  def main(a:Array[String]) = {
    val data = read(a(0))
    write(permutate(data, data.length))
  }

  def permutate(data: List[Rec], n: Int): List[List[Rec]] = {
    n match {
      case 1 => List(data)
      case _ =>
        val p = permutate(data.tail, n-1)
        if (p.isEmpty) p
        else upgrade(data.head, p).filter(check)
    }
  }

  def upgrade(x: Rec, cur: List[List[Rec]]) : List[List[Rec]] = {
      for {list <- cur
           pos <- 0 to list.length} yield insert(x, list, pos)
  }

  def insert(x: Rec, list: List[Rec], pos: Int): List[Rec] = {
    list.slice(0, pos) ::: (x :: list.slice(pos, list.length))
  }

  def check(p: List[Rec]) : Boolean = {
    p.length match {
      case 2 => check2(p, 0)
      case 4 => check4(p, 0)
      case 5 => check5(p, 0)
      case 6 => check6(p, 0)
      case 7 => check7(p, 0)
      case 8 => check8(p, 0)
      case 9 => check9(p, 0)
      case 10 => check10(p, 0)
      case 11 => check11(p, 0)
      case 12 => check12(p, 0)
      case _ => true
    }
  }

  def check2(p:List[Rec], s:Int):Boolean = {
    p(s).dr + p(s + 1).dl < 10
  }

  def check4(p:List[Rec], s:Int):Boolean = {
    check2(p, s + 2) && p(s).dr + p(s + 1).dl + p(s + 3).ur < 10
  }

  def check5(p:List[Rec], s:Int):Boolean = {
    check4(p, s + 1) && p(s).dr + p(s + 1).dl + p(s + 3).ur + p(s + 4).ul == 10
  }

  def check6(p:List[Rec], s:Int):Boolean = {
    check5(p, s + 1) && p(s).dr + p(s + 1).dl + p(s + 4).ur < 10
  }

  def check7(p:List[Rec], s:Int):Boolean = {
    check6(p, s + 1) && p(s).dr + p(s + 4).ur < 10
  }

  def check8(p:List[Rec], s: Int):Boolean = {
    check7(p, s + 1) && p(s).dr + p(s + 1).dl + p(s + 4).ur + p(s + 5).ul == 10
  }

  def check9(p:List[Rec], s:Int):Boolean = {
    check8(p, s + 1) && p(s).dr + p(s + 1).dl + p(s + 4).ur + p(s + 5).ul == 10
  }

  def check10(p:List[Rec], s:Int):Boolean = {
    check9(p, s + 1) && p(s).dr + p(s + 1).dl + p(s + 4).ur + p(s + 5).ul == 10 && p(s).dl + p(s + 4).ul < 10
  }

  def check11(p:List[Rec], s:Int):Boolean = {
    check10(p, s + 1) && p(s).dr + p(s + 3).ur + p(s + 4).ul < 10
  }

  def check12(p:List[Rec], s:Int):Boolean = {
    check11(p, s + 1) && p(s).ur + p(s + 1).ul < 10 &&
    p(s).dr + p(s + 1).dl + p(s + 3).ur + p(s + 4).ul == 10 &&
    p(s).dl + p(s + 2).ur + p(s + 3).ul < 10
  }

  def read(filename:String) : List[Rec] = {
    Source.fromFile(filename).getLines.collect{case line: String => Rec.fromLine(line)}.toList
  }

  def write(data: List[List[Rec]]) = {
    data match {
      case _ :: _ =>
        data.foreach(x => {
          x.foreach(println(_))
          println
        })
      case Nil => println("No solution")
    }
   }
}
