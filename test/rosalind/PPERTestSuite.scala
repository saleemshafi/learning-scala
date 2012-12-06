package rosalind

import org.scalatest.FunSuite
import rosalind.PPER._
import scala.collection.mutable.Stack
 
class TRANTestSuite extends FunSuite {
  test("permutations with data from problem") {
	expect(3360) { numPermutationsMod(16, 3, 1000000) }
	expect(108800) { numPermutationsMod(87, 10, 1000000) }
  }

  test("permutations with real data") {
	expect(108800) { numPermutationsMod(85, 8, 1000000) }
  }
}