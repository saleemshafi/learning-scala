package rosalind

import org.scalatest.FunSuite
import rosalind.CONV._
import scala.collection.mutable.Stack
 
class CONVTestSuite extends FunSuite {
	def getMultiSet(d: String):MultiSet[BigDecimal] =
	  d.split(" ") map (a => BigDecimal.apply(a))
  
  test("spectral convolution with data from problem") {
    val multiSet1 = getMultiSet("186.07931 287.12699 548.20532 580.18077 681.22845 706.27446 782.27613 968.35544 968.35544")
	val multiSet2 = getMultiSet("101.04768 158.06914 202.09536 318.09979 419.14747 463.17369")    
	val diff = minkowskiDifferenceMap(multiSet1, multiSet2)
	expect((BigDecimal.apply("85.03163"), 3)) { diff.maxBy(item => item._2) }
  }
}