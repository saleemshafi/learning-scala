package rosalind

import scala.math.BigDecimal.RoundingMode
import scala.math._

object PROB2 extends App with RosalindProblem {
	def logProbability(s:String)(gc:Double) = {
	  val gcProb = gc / 2
	  val atProb = (1-gc) / 2
	  val numGC = s.count(x => x == 'G' || x =='C')
	  val numAT = s.count(x => x == 'A' || x == 'T')
	  val probS = Math.pow(gcProb, numGC) * Math.pow(atProb, numAT)
	  BigDecimal.apply(log10(probS)).setScale(3, RoundingMode.HALF_UP)
	}
  
}