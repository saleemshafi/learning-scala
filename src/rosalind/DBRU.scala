package rosalind

object DBRU extends App with RosalindProblem {
	def getDeBrujinGraph(k1mers:Iterable[String]):Set[(String,String)] = {
		val SuSrc = (k1mers ++ (k1mers map (a => REVC.reverseComplement(a)))).toSet
		(for(r <- SuSrc) yield (r.init, r.tail)).toSet
	}
	
}