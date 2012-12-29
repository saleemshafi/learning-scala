package rosalind

import org.scalatest.FunSuite
import rosalind.NWCK._
import rosalind.QRTTestSuite._

import scala.collection.mutable.Stack
 
class CUNRTestSuite extends FunSuite with RosalindProblem {
  
  
  // http://en.wikipedia.org/wiki/Unrooted_binary_tree#Enumeration
  def getNumUBTreesWithXNodes(n:Int):Int = {
    val s = n-2
    val e = (2*n)-4
    val numerator = (s+1 to e).foldLeft(BigInt.apply(1))( (a:BigInt, b:Int) => (a * b) )
    val denominator = (1 to n-2).foldLeft(BigInt.apply(1))( (a:BigInt, b:Int) => (a * 2) )
    ((numerator / denominator) % 1000000).intValue
  }

  def getNumRBTreesWithXNodes(n:Int):Int = {
    (1 to (2*n - 2)).foldLeft(1)((a:Int, b:Int) => if (b % 2 == 0) a else (a *b) % 1000000 )
  }
  
  test("unrooted binary tree count") {
//    expect(1) { getNumTreesWithXNodes(2) }
    expect(1) { getNumUBTreesWithXNodes(3) }
    expect(3) { getNumUBTreesWithXNodes(4) }
    expect(15) { getNumUBTreesWithXNodes(5) }
    expect(105) { getNumUBTreesWithXNodes(6) }
    expect(945) { getNumUBTreesWithXNodes(7) }
    expect(10395) { getNumUBTreesWithXNodes(8) }
    expect(135135) { getNumUBTreesWithXNodes(9) }
    expect(27025) { getNumUBTreesWithXNodes(10) }
    expect(609375) { getNumUBTreesWithXNodes(953) }
  }
  
  test("rooted binary tree count") {
    expect(15) { getNumRBTreesWithXNodes(4) }
    expect(859375) { getNumRBTreesWithXNodes(972) }
  }
}