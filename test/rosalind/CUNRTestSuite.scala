package rosalind

import org.scalatest.FunSuite
import rosalind.NWCK._
import rosalind.QRTTestSuite._

import scala.collection.mutable.Stack
 
class CUNRTestSuite extends FunSuite with RosalindProblem {
  
  
  // http://en.wikipedia.org/wiki/Unrooted_binary_tree#Enumeration
  def getNumTreesWithXNodes(n:Int):Int = {
    val s = n-2
    val e = (2*n)-4
    val numerator = (s+1 to e).foldLeft(BigInt.apply(1))( (a:BigInt, b:Int) => (a * b) )
    val denominator = (1 to n-2).foldLeft(BigInt.apply(1))( (a:BigInt, b:Int) => (a * 2) )
    ((numerator / denominator) % 1000000).intValue
  }
  
  test("tree count") {
//    expect(1) { getNumTreesWithXNodes(2) }
    expect(1) { getNumTreesWithXNodes(3) }
    expect(3) { getNumTreesWithXNodes(4) }
    expect(15) { getNumTreesWithXNodes(5) }
    expect(105) { getNumTreesWithXNodes(6) }
    expect(945) { getNumTreesWithXNodes(7) }
    expect(10395) { getNumTreesWithXNodes(8) }
    expect(135135) { getNumTreesWithXNodes(9) }
    expect(27025) { getNumTreesWithXNodes(10) }
    expect(609375) { getNumTreesWithXNodes(953) }
  }
}