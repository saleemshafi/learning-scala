package rosalind

object REVC extends App with RosalindProblem {

	def reverseComplement(s:String):String =
	  s.reverse map ((c:Char) => c match {
	    case 'T'=> 'A'
	    case 'A'=> 'T'
	    case 'G'=> 'C'
	    case 'C'=> 'G'
	  })
  
	runWithInput("REVC.input")(reverseComplement)

}