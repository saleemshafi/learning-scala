package rosalind

object DNA extends App with RosalindProblem {
	def pack(s:List[Char]): List[List[Char]] = s match {
	  case Nil => List()
	  case x::xs => {
	    val (start, rest) = s span (y => y == x)
	    start :: pack(rest)
	  }
	}

	def countNucleotides(dna: String): String = 
	  (pack((dna toList) sorted) map (n => n.length)).mkString(" ")

	runWithInput("DNA.input")(countNucleotides)
}