package rosalind

import scala.math.BigDecimal.RoundingMode

object LONG extends App with RosalindProblem {
  def halfLength(s:String) = (s.length / 2) + 1
  
  def compress(dna:List[(String, String, String)]):String = dna match {
    case Nil => ""
    case a::Nil => a._3
    case first::rest => {
      val potentialOverlaps = rest.zipWithIndex.partition(dnaIdx => dnaIdx._1._3.indexOf(first._1) > -1)._1
      potentialOverlaps match {
        case Nil => compress(rest:+first)
        case overlaps => {
          val bestMatch = overlaps.minBy(item => item._1._3.indexOf(first._1) )
          val merged = bestMatch._1._3.substring(0, bestMatch._1._3.indexOf(first._1)) + first._3
          compress(rest.updated(bestMatch._2, (bestMatch._1._1 , first._2 , merged) ))
        }
      }
    }
  }

  val data = getInput("LONG.input")
  val dnaWithPreSuff = data.map(dna => (dna.take(halfLength(dna)), dna.takeRight(halfLength(dna)), dna))
  println(compress(dnaWithPreSuff))
}