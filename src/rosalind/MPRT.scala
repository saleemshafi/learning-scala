package rosalind

import scala.util.matching.Regex
import scala.io.Source

object MPRT extends App with RosalindProblem {
	def findOccurrences(p:Regex)(proteinId:String):Seq[Int] = {
	  val proteinFasta = Source.fromURL("http://www.uniprot.org/uniprot/"+proteinId+".fasta").mkString
	  val (pId, data) = GC.parseFASTA(proteinFasta.split('\n').toList).head
	  (for (m <- p.findAllIn(data).matchData) yield m.start+1).toList
	}
  
}