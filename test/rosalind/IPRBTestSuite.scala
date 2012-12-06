package rosalind

import org.scalatest.FunSuite
import rosalind.IPRB._
import scala.collection.mutable.Stack
 
class IPRBTestSuite extends FunSuite {
  test("Mendel's Law with data from problem") {
	expect(BigDecimal.apply("0.78333")) { chanceOfDominantPhenotype(2, 2, 2) }
  }

  test("Mendel's Law with real data") {
	expect(BigDecimal.apply("0.75813")) { chanceOfDominantPhenotype(31151, 39634, 29507) }
  }

}