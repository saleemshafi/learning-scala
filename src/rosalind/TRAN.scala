package rosalind

import scala.math.BigDecimal.RoundingMode

object TRAN extends App with RosalindProblem {
  def transitionCount(s1:String, s2:String):Int = 
    s1.zip(s2).count { 
    	case ('A', 'G') | ('G', 'A') => true
    	case ('C', 'T') | ('T', 'C') => true
    	case _ => false
  	}
  
  def transversionCount(s1:String, s2:String):Int =
    s1.zip(s2).count { 
    	case ('A', x) => x == 'C' || x == 'T'
    	case ('G', x) => x == 'C' || x == 'T'
    	case ('T', x) => x == 'A' || x == 'G'
    	case ('C', x) => x == 'A' || x == 'G'
    }
  
  def ttRatio(s1:String, s2:String):BigDecimal = 
    BigDecimal.apply(Double.box(transitionCount(s1, s2)) / Double.box(transversionCount(s1, s2))).setScale(5, RoundingMode.HALF_UP)
}