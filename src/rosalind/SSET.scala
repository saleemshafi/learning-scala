package rosalind

import scala.math.BigDecimal.RoundingMode

object SSET extends App with RosalindProblem {
	def pascalsTriangle():Stream[List[Long]] = {
	  def pascal(prevRow:List[Long]):Stream[List[Long]] = {
	    val thisRow = (for(pair <- ((0L::prevRow) :+ 0L).sliding(2)) yield (pair.sum % 1000000)).toList
	    thisRow #:: pascal(thisRow)
	  }
	  List(1L) #:: pascal(List(1L))
	}
  
	def numSubsets(n: Int):Long = 
	  pascalsTriangle()(n) reduceLeft { (a,b) => (a + b) % 1000000 }
	  
	println(numSubsets(962))
}