package rosalind

object RNA extends App with RosalindProblem {

	def rnaTranscription(s:String):String =
	  s map (c => c match {
	    case 'T'=> 'U'
	    case _ => c
	  })
  
	runWithInput("RNA.input")(rnaTranscription)
}