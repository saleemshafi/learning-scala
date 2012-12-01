package rosalind

import scala.math.BigDecimal.RoundingMode

object CORR extends App with RosalindProblem {
  def groupCommonSequences(read:Seq[String]):Map[Set[String], Seq[String]] =
    read.groupBy(s => Set(s, REVC.reverseComplement(s)))
    
  def partitionErrors(reads:Seq[String]):(Iterable[String], Iterable[String]) = {
    reads.partition( s => {
      val s1 = REVC.reverseComplement(s)
      reads.count(a => a.equals(s) || a.equals(s1)) == 1 
    } )
    
//    val (errorMap, correctMap) = groupCommonSequences(reads).partition(gr => gr._2.length == 1)
/*    
    println("Errors:")
    errorMap foreach println
    println("Corrections:")
    correctMap foreach println
*/
//    (for(e <- errorMap) yield e._2.head, (for(c <- correctMap; ac <- c._2) yield ac).toSet)
  }
  
  def getPossibleCorrections(main:String, others:Iterable[String]):Iterable[String] =
    for (other <- others; if (!main.equals(other)); or <- List(other, REVC.reverseComplement(other));if (1 == HAMM.hammingWeight(or, main))) yield or
  
  def getCorrections(reads:Seq[String]):Iterable[(String, String)] = {
      val (errors, corrects) = partitionErrors(reads)
//      errors.foreach { case err => println(err+":"+getPossibleCorrections(err, corrects)) }
      (for (err <- errors) yield (err, getPossibleCorrections(err, corrects).head))
  }
}