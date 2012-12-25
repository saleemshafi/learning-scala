package rosalind

import rosalind.SPEC._

object SGRA extends App with RosalindProblem {
    type Edge = (BigDecimal, BigDecimal, Char)
  
	def getProteinStrings(graph:Map[BigDecimal, Seq[Edge]], start:BigDecimal):Seq[String] = 
	  graph.get(start) match {
	      case None => List("")
	      case Some(edges) => for ((x, node, ch)  <- edges; 
	    							suffix <- getProteinStrings(graph, node)) yield ch + suffix
      }
    
    def findProtein(mass:BigDecimal):Iterable[Char] = {
    	for ((p, m) <- PRTM.monoisotopicMass; if ((mass - m).abs < 0.01)) yield p
    }
  
	def getProteinStringFromMasses(masses: Seq[BigDecimal]):String = {
	  val edges = (for (a <- masses; b <- masses; protein <- findProtein(b - a))
		  yield (a, b, protein)).groupBy(a => a._1)
	  
	  val allStrings = for (start <- masses; str <- getProteinStrings(edges, start)) yield str
	  allStrings.maxBy(str => str.length())
	}
}