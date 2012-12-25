package rosalind

object ASPC extends App with RosalindProblem {
  def numPermutations(n:BigInt, r:BigInt):BigInt = 
	  (n-r+1 to n).reduceLeft( (a:BigInt,b:BigInt) => (a * b))

  def choose(n:BigInt, r:BigInt):BigInt = {
	  val perms = numPermutations(n, r)
	  (BigInt.apply(2) to r).foldLeft(perms)((a,b) => a / b )
	}
	  
  def chooseMod(n:BigInt, r:BigInt, mod:Int):Int = {
	  val perms = numPermutations(n, r)
	  ((BigInt.apply(2) to r).foldLeft(perms)((a,b) => a / b ) % mod).intValue
	}

	def sumCombos(n:Int, m:Int, mod:Int):Int = 
	  (for(k <- m to n) yield chooseMod(n, k, mod)).reduceLeft((a,b) => (a+b) % mod)
}