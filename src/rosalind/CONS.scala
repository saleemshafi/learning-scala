package rosalind

object CONS extends App with RosalindProblem {
	type ProfileMatrix = Map[Char, List[Int]]
	val NucleicAcidBases = "ACGT"
	
	def profileMatrix(dnaStrings:List[String]):ProfileMatrix = {
	  def profileMatrix(acids:List[Char]):ProfileMatrix = 
	    acids.groupBy(c => c).map(counts => (counts._1 -> List(counts._2.length))).withDefaultValue(List(0))
	  
	  def mergeMatrix(a:ProfileMatrix, b:ProfileMatrix):ProfileMatrix =
	    a.map(entry => (entry._1 -> (entry._2 ::: b(entry._1))))

	  def sliceStrHead(strings:List[String]):(List[Char], List[String]) =
	    (for (str <- strings) yield (str.head, str.tail)).unzip
	    
	  def profileMatrix1(dnaStrings:List[String], matrix:ProfileMatrix):ProfileMatrix = dnaStrings.head match {
	    case "" => matrix
	    case _ => {
	      val (heads, tails) = sliceStrHead(dnaStrings)
	      profileMatrix1(tails, mergeMatrix(matrix, profileMatrix(heads)))
	    }
	  }
	  val matrix = NucleicAcidBases.map(base => (base -> List())).toMap.withDefaultValue(List(0))
	  profileMatrix1(dnaStrings, matrix)
	}
  
	def consensusString(matrix:ProfileMatrix):String = {
	  def sliceHead[T](lists:List[List[T]]):(List[T], List[List[T]]) =
	    (for (lst <- lists) yield (lst.head, lst.tail)).unzip
	    
	  def getMaxBase(counts:List[Int]):Char =
	    NucleicAcidBases.zip(counts).maxBy(a => a._2)._1
	    
	  def consensusString1(matrix:List[List[Int]], consensus:String):String = matrix.head match {
	    case Nil => consensus
	    case _ => {
	      val (heads, tails) = sliceHead(matrix)
	      consensusString1(tails, consensus + getMaxBase(heads))
	    }
	  }
	  
	  val counts = (for (base <- NucleicAcidBases) yield matrix(base)).toList
	  consensusString1(counts, "")
	}
	
	val profile = profileMatrix(getInput("CONS.input"))
	println(consensusString(profile))
	NucleicAcidBases.foreach {
	    base => println(base+": "+profile(base).mkString(" "))
	  }
}