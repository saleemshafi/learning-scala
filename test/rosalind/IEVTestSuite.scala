package rosalind

import org.scalatest.FunSuite
import rosalind.IEV._
import scala.collection.mutable.Stack
 
class IEVTestSuite extends FunSuite with RosalindProblem {
  test("calculating offspring with data from problem") {
    expect(BigDecimal.apply("3.5")) { numChildrenWithDomPhenotype((1, 0, 0, 1, 0, 1)) }
  }

  test("calculating offspring with real data") {
    expect(BigDecimal.apply("149727.5")) { numChildrenWithDomPhenotype((16361, 17434, 18057, 19431, 16877, 18986)) }
  }
}