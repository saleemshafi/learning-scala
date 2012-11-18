package rosalind

object KMER extends App with RosalindProblem {
  
	def kmerCounts(dna: String)(k: Int):Map[String, Int] = 
	  dna.sliding(k).toList.groupBy(identity).map(e => (e._1 -> e._2.length)) withDefaultValue 0
	
	def kmerComposition(dna: String)(k: Int):List[Int] = {
	  val counts = kmerCounts(dna)(k)
	  val perms = LEXF.permutations("ACGT".toSet)(k).sort(LEXF.lexSort("ACGT".toList))
	  for(kmer <- perms) yield counts(kmer.mkString)
	  
	}
  
	val dnaInFasta = GC.parseFASTA(getInput("KMER.input"))
	println(kmerComposition(dnaInFasta.head._2)(4).mkString(" "))
}