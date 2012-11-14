package rosalind

object HAMM extends App with RosalindProblem {
	def hammingWeight(s: String, t: String) = 
	  ((s zip t) map (pair => if (pair._1 == pair._2) 0 else 1)).reduceLeft((a,b) => a + b)
  
	val data = getInput("HAMM.input")
	(for (pairs <- data.grouped(2); if pairs.length == 2) 
	  yield hammingWeight(pairs(0), pairs(1))) foreach print
}