package rosalind

import scala.util.matching.Regex
import scala.io.Source

object MPRT extends App with RosalindProblem {
	def findOccurrences(p:Regex)(proteinId:String):Seq[Int] = {
	  
	  def findIndexes(indexes: Seq[Int], data:String, p:Regex, offset:Int):Seq[Int] = {
		 p.findFirstMatchIn(data.substring(offset)) match {
		   case None => indexes
		   case Some(m) => findIndexes(indexes:+(offset+m.start+1), data, p, offset+m.start+1)
		 }
	  }
	  
	  val proteinFasta = Source.fromURL("http://www.uniprot.org/uniprot/"+proteinId+".fasta").mkString
	  val (pId, data) = GC.parseFASTA(proteinFasta.split('\n').toList).head
	  
	  findIndexes(Nil, data, p, 0)
	}
  
}