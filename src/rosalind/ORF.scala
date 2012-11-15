package rosalind

import Stream._

object ORF extends App with RosalindProblem {
	def getOpenReadingFrames(dna:String):Seq[String] = {
	  var revComp = REVC.reverseComplement(dna)
	  var frames = dna::dna.tail::dna.tail.tail::revComp::revComp.tail::revComp.tail.tail::Nil
	  var framePattern = """(?:M[^\.]*)\.""".r
	  for (frame <- frames; 
			  piece <- framePattern.findAllIn(PROT.encodeDNA(frame));
			  index <- KMP.getIndexesFromEnd("M", piece))
	    yield piece.substring(index).init
	}
  
	getInput("ORF.input").foreach( dna => {
	  getOpenReadingFrames(dna).distinct foreach println
	})
}