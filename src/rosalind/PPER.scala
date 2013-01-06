package rosalind

object PPER extends App with RosalindProblem {
	def numPermutationsMod(n:Int, r:Int, mod:Int):Int = 
	  (n-r+1 to n).reduceLeft( (a:Int,b:Int) => ((a * b) % mod) )
  
}