package rosalind

object MRNA extends App with RosalindProblem {
	val codonReversals = codon_table.groupBy((p: (String,Char)) => p._2).map(g => (g._1, g._2.keySet.size))
	
	def numReversals(proteinStr: String):String = 
	  (proteinStr.map(c => codonReversals(c)).foldLeft(3)((a,b) => (a * b) % 1000000 )).toString
	
	runWithInput("MRNA.input")(numReversals)
}
