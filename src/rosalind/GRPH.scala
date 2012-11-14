package rosalind

import GC.parseFASTA

object GRPH extends App with RosalindProblem {

  def getAdjacencies(k:Int)(dnaInFASTA:List[(String,String)]): List[(String, String)] = {
    val heads = dnaInFASTA.groupBy(fasta => fasta._2.substring(fasta._2.length-k)) withDefaultValue List()
    val tails = dnaInFASTA.groupBy(fasta => fasta._2.substring(0,k)) withDefaultValue List()
    (for (overlap <- heads.keys; h <- heads(overlap); t <- tails(overlap); if (h._1 != t._1))
      yield (h._1, t._1)).toList
  }
  
  val dnaStrings = parseFASTA(getInput("GRPH.input"))
  val adjacencies = getAdjacencies(3)(dnaStrings)
  
  adjacencies.foreach(pair => println(pair._1+" "+pair._2)) 
}