package rosalind

object SSEQ extends App with RosalindProblem {
	def subsequenceIndexes[T](sub:List[T], str:List[T], offset:Int):List[Int] = sub match {
	  case Nil => Nil
	  case h::t => {
	    val i = str.indexOf(h)+1
	    (offset+i)::subsequenceIndexes(t, str.splitAt(i)._2, i+offset)
	  }
	}
  
	getInput("SSEQ.input").grouped(2).foreach( {
	  case List(str, sub) => println(subsequenceIndexes(sub.toList, str.toList, 0).mkString(" "))
	})
}