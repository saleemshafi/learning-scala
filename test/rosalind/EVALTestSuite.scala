package rosalind

import org.scalatest.FunSuite
import rosalind.NWCK._
import rosalind.QRTTestSuite._
import scala.math.BigDecimal.RoundingMode
 
class EVALTestSuite extends FunSuite with RosalindProblem {
  def getNumRestrictionSites(len:Int, str:String, gcRates:Seq[BigDecimal]):Seq[BigDecimal] =
    for (gc <- gcRates) yield getNumRestrictionSites(len, str, gc)
  
  def getProbability(c:Char, gcRate:BigDecimal):BigDecimal = c match {
      case 'A' | 'T' => (1 - gcRate) / 2
      case 'G' | 'C' => gcRate / 2
      case _ => 0
    }
  
  def getProbability(str:String, gcRate:BigDecimal):BigDecimal = 
    str.foldLeft[BigDecimal](1)((r,c) => r * getProbability(c, gcRate))
  
  def getNumRestrictionSites(len:Int, str:String, gcRate:BigDecimal):BigDecimal = 
    (getProbability(str, gcRate) * (len - 1)).setScale(3, RoundingMode.HALF_UP)
  
  test("num restriction sites with data from problem") {
    val data = """10
AG
0.25 0.5 0.75""".split("\r\n").toList
	val strLength = Integer.parseInt(data.head)
	val needle = data.tail.head
	val gcRates = data.last.split(" ") map (BigDecimal apply)
	
	expect(List(BigDecimal("0.422"),BigDecimal("0.563"),BigDecimal("0.422"))) { getNumRestrictionSites(strLength, needle, gcRates).toList }
  }

  test("num restriction sites with real data") {
    val data = """826009
GTGCCTAT
0.000 0.073 0.141 0.229 0.274 0.347 0.383 0.428 0.517 0.532 0.621 0.694 0.743 0.769 0.837 0.926 1.000""".split("\r\n").toList
	val strLength = Integer.parseInt(data.head)
	val needle = data.tail.head
	val gcRates = data.last.split(" ") map (BigDecimal apply)
	
	println (getNumRestrictionSites(strLength, needle, gcRates).mkString(" "))
  }

}