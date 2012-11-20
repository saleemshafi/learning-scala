package rosalind
import scala.math.BigDecimal.RoundingMode

object PDST extends App with RosalindProblem {
  
  def pDistance(s1:String, s2:String):BigDecimal =
    ((s1 zip s2) map (p => if (p._1 == p._2) 0 else 1)).sum / s1.length.toDouble
    
  val fastaData = GC.parseFASTA(getInput("PDST.input"))
  
  println( (for(j <- 0 to fastaData.length-1) yield {
    (for (i <- 0 to fastaData.length-1) yield pDistance(fastaData(i)._2, fastaData(j)._2).setScale(3, RoundingMode.HALF_UP)).mkString(" ")
  }).mkString("\n"))
}