package rosalind

object SPLC extends App with RosalindProblem {
	def splice(dna: String, introns:List[String]): String = introns match {
	  case Nil => dna.replace(" ", "")
	  case intron::rest => splice(dna.replace(intron, " "), rest)
	}
		
	
	val data = getInput("SPLC.input")
	println(PROT.encodeDNA(splice(data.head, data.tail)).init)
}