package rosalind

object PERM extends App with RosalindProblem {
	def permutations[T](elements: List[T]):List[List[T]] = elements match {
	  case Nil => List(List())
	  case h::t => for (prevSeq <- permutations(t); index <- 0 to prevSeq.length)
	    yield prevSeq.patch(index, List(h), 0)
	} 
  
	getInput("PERM.input").foreach( input => {
		val perms = permutations((1 to input.trim.toInt).toList)
	    println (perms.length)
	    perms foreach (l => println(l.mkString(" ")))
	})
	
}