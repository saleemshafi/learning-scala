package rosalind

import scala.math.BigDecimal.RoundingMode

object EVAL extends App with RosalindProblem {
  def substringProbability(m:Int, n:Int)(gc:Double):BigDecimal = 
    Math.pow(PROB.sameTwoProbability(gc), m)*(n-m+1)
  
  val data = getInput("EVAL.input")
  val sizes = data.head.split(" ")
  val sp = substringProbability(sizes.head.toInt, sizes.tail.head.toInt) _
  println( data.tail.head.split(" ").map(gc => sp(gc.toDouble).setScale(5, RoundingMode.HALF_UP)).mkString(" "))
}