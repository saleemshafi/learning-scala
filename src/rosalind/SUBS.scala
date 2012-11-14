package rosalind

import Stream._

object SUBS extends App with RosalindProblem {
	def findSubstrings(s: String, t: String):Stream[Int] = findSubstrings(s, t, 0)
	
	def findSubstrings(s: String, t: String, start: Int):Stream[Int] = {
	  val index = s.indexOf(t, start)
	  if (index < 0) empty
	  else (index+1) #:: findSubstrings(s, t, index+1)
	}
  
	val data = getInput("SUBS.input")
	println(((for (pairs <- data.grouped(2); if pairs.length == 2; index <- findSubstrings(pairs(0), pairs(1))) 
	  yield index).toList).mkString(" "))
}