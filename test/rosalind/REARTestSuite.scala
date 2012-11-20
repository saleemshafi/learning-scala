package rosalind

import org.scalatest.FunSuite
import rosalind.REAR._
import scala.collection.mutable.Stack
 
class REARTestSuite extends FunSuite {
  def getDataset(data:String) = 
    (data.split(" ") map (a => a.toInt)).toList
  
  test("reversal distance with data (set #1) from problem") {
    val start = getDataset("1 2 3 4 5 6 7 8 9 10")
    val end = getDataset("3 1 5 2 7 4 9 6 10 8")
	expect(9) { getNumReversals(start, end) }
  }

  test("reversal distance with data (set #2) from problem") {
    val start = getDataset("3 10 8 2 5 4 7 1 6 9")
    val end = getDataset("5 2 3 1 7 4 10 8 6 9")
	expect(4) { getNumReversals(start, end) }
  }

  test("reversal distance with data (set #3) from problem") {
    val start = getDataset("8 6 7 9 4 1 3 10 2 5")
    val end = getDataset("8 2 7 6 9 1 5 3 10 4")
	expect(5) { getNumReversals(start, end) }
  }

  test("reversal distance with data (set #4) from problem") {
    val start = getDataset("3 9 10 4 1 8 6 7 5 2")
    val end = getDataset("2 9 8 5 1 7 3 4 6 10")
	expect(7) { getNumReversals(start, end) }
  }

  test("reversal distance with data (set #5) from problem") {
    val start = getDataset("1 2 3 4 5 6 7 8 9 10")
    val end = getDataset("1 2 3 4 5 6 7 8 9 10")
	expect(0) { getNumReversals(start, end) }
  }
}