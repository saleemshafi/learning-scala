package rosalind

object REVP extends App with RosalindProblem {
	def getPalindromes() = {
	  val perms = LEXF.permutations("TAGC".toList.toSet) _
	  ((perms(2)++perms(3)++perms(4)) map (segment => segment.mkString+REVC.reverseComplement(segment.mkString))).toSet
	}
  
	def getPalindromeLocations(str:String): List[(Int, String)] = 
	  for (p <- getPalindromes().toList; i <- KMP.getIndexesFromEnd(p, str)) yield (i, p)
  
	getInput("REVP.input").foreach( str => {
	  getPalindromeLocations(str).sortBy(p => p._1)(Ordering.Int).foreach( palLoc => {
	    println((palLoc._1+1)+" "+palLoc._2.length)
	  })
	})
	
}