package rosalind

import scala.math.BigDecimal.RoundingMode

object CONV extends App with RosalindProblem {
  type MultiSet[T] = Seq[T]
  
  def minkowskiDifference(s1:MultiSet[BigDecimal], s2:MultiSet[BigDecimal]):MultiSet[BigDecimal] = 
    for (item1 <- s1; item2 <- s2) yield (item1 - item2)

  def minkowskiDifferenceMap(s1:MultiSet[BigDecimal], s2:MultiSet[BigDecimal]):Map[BigDecimal, Int] = 
    minkowskiDifference(s1, s2).groupBy(identity) map { case (item, list) => (item -> list.length)}
  
}