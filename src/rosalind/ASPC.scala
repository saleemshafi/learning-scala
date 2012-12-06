package rosalind

object ASPC extends App with RosalindProblem {
	def chooseMod(n:Int, r:Int, mod:Int):Int = {
	  val perms = PPER.numPermutationsMod(n, r, mod)
	  (2 to r).foldLeft(perms)((a,b) => a / b )
	}

	def sumCombos(n:Int, m:Int, mod:Int):Int = 
	  (for(k <- m to n) yield chooseMod(n, k, mod)).sum
}