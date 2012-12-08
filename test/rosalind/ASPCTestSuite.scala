package rosalind

import org.scalatest.FunSuite
import rosalind.ASPC._
import scala.collection.mutable.Stack
 
class ASPCTestSuite extends FunSuite {
  test("combinations with data from problem") {
	expect(560) { chooseMod(16, 3, 1000000) }
	expect(42) { sumCombos(6,3, 1000000)}
  }

  test("permutations with real data") {
	expect(586368) { sumCombos(1606, 618, 1000000)}
	expect(324344) { sumCombos(1734, 1315, 1000000)}
  }
}