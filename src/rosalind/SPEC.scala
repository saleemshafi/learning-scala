package rosalind

import scala.math.BigDecimal
import scala.math.BigDecimal.RoundingMode

object SPEC extends App with RosalindProblem {
  val massLookup = PRTM.monoisotopicMass.map(mass => (scaled(mass._2) -> mass._1))
  
  def scaled(value:BigDecimal) = value.setScale(2, RoundingMode.HALF_UP)
  
  def proteinFromSpectrum(spectrum:List[Double]):String = 
    (for(weight <- spectrum.sliding(2).map({ case a::(b::rest) => b - a})) yield massLookup(scaled(weight))).mkString
  
  val prefixSpectrum = getInput("SPEC.input")
  println(proteinFromSpectrum(prefixSpectrum.map(a => a.toDouble)))
}