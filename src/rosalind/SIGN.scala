package rosalind

import scala._

object SIGN extends App with RosalindProblem {
  
	def signedPermutations(elements: List[Int]):List[List[Int]] = elements match {
	  case Nil => List(List())
	  case h::t => for (prevSeq <- signedPermutations(t); 
			  			multiplier <- List(1, -1);
			  			index <- 0 to prevSeq.length)
	    yield prevSeq.patch(index, List(multiplier * h), 0)
	} 
  
	val perms = signedPermutations((1 to 6).toList)
    println (perms.length)
    perms foreach (l => println(l.mkString(" ")))
	
}