package rosalind

import Stream._

object KMP extends App with RosalindProblem {
  
	def getIndexesFromEnd(needle:String, haystack:String):Stream[Int] = {
	  val lastIndex = haystack.lastIndexOf(needle)
	  if (lastIndex == -1) empty
	  else if (lastIndex == 0) lastIndex #:: empty
	  else lastIndex #:: getIndexesFromEnd(needle, haystack.substring(0, lastIndex));
	}
  
	def suffixMatchLength(s:String):Int = 
	  (for (index <- getIndexesFromEnd(s.takeRight(1), s.init); 
			  prefix = s.take(index+1);
			  if (s.endsWith(prefix))) yield prefix) match {
	  		case Stream() => 0
	  		case firstMatch #:: more => firstMatch.length
		}
  
	def failureArray(dna: String): Seq[Int] =
	  for (k <- 1 to dna.length) yield suffixMatchLength(dna.take(k))

	def failureArrayAsString(dna: String): String =
	  failureArray(dna).mkString(" ")
	  
	runWithInput("KMP.input")(failureArrayAsString)
}