package rosalind

import scala.math.BigDecimal.RoundingMode

object SETO extends App with RosalindProblem {
  def getSet(setStr:String):Set[Int] =
    (setStr.tail.init.split(", ") map (a => a toInt)) toSet
    
  def mkString(set:Set[Int]):String = 
    "{" + set.mkString(", ") + "}"
  
  val data = getInput("SETO.input")
  val U = (1 to data(0).toInt).toSet
  val A = getSet(data(1))
  val B = getSet(data(2))
  
  println(mkString(A union B))
  println(mkString(A intersect B))
  println(mkString(A diff B))
  println(mkString(B diff A))
  println(mkString(U diff A))
  println(mkString(U diff B))
}