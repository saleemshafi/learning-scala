package rosalind

object PPER extends App with RosalindProblem {
	def numPermutationsMod(k:Int, n:Int, mod:Int):Int = 
	  (k-n+1 to k).reduceLeft( (a:Int,b:Int) => ((a * b) % mod) )
  
}