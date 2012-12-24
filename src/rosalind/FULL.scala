package rosalind

import rosalind.SPEC._

object FULL extends App with RosalindProblem {
  
  def findProtein(s1:BigDecimal, masses:Seq[BigDecimal]):(Char, Seq[BigDecimal]) = {
    val lookup = PRTM.monoisotopicMass.map(mass => (scaled(mass._2) -> mass._1)) withDefaultValue '_'
    lookup( scaled(masses.head - s1) ) match {
      case '_' => findProtein(s1, masses.tail)
      case ch => (ch, masses)
    }
  }
  
  def proteinFromFullSpectrum(parentMass:BigDecimal, spectrum:Seq[BigDecimal]):String = {
    val sortedSpectrum = spectrum.sorted(Ordering.BigDecimal)
    val n = (sortedSpectrum.length - 2) / 2
    
    def findProteins(proteins:String, masses:Seq[BigDecimal]):String = {
      if (proteins.length == n) proteins
      else {
	      val (ch, rest) = findProtein(masses.head, masses.tail)
	      findProteins(proteins+ch, rest)
      }
    }
    
    findProteins("", sortedSpectrum)
  }
}