package rosalind

import scala.math.BigDecimal.RoundingMode
import rosalind.CONV._

object PRSM extends App with RosalindProblem {
  
  def getAllPrefixes(str:String) = for (i <- (1 to str.length-1)) yield str.view(0, i).mkString
  def getAllSuffixes(str:String) = for (i <- (1 to str.length-2)) yield str.view(i, str.length-1).mkString
  
  def getWeight(str:String) = 
    BigDecimal.apply((for (c <- str) yield PRTM.monoisotopicMass(c)).sum).setScale(5, RoundingMode.HALF_UP)
  
  def getCompleteSpectrum(protein:String):MultiSet[BigDecimal] =
    for (segment <- (getAllPrefixes(protein) ++ getAllSuffixes(protein))) yield getWeight(segment)
    
  def getMatchingProtein(proteins:Set[String], spectrumR:MultiSet[BigDecimal]):(String, Int) = {
    val maxForEachProtein = for (prot <- proteins.toList; 
    							spectrumP = getCompleteSpectrum(prot)) 
    						yield (prot, maxOccurring(minkowskiDifference(spectrumR, spectrumP)))
    val maxVal = maxForEachProtein maxBy (a => a._2._2)
    (maxVal._1, maxVal._2._2)
  } 
}