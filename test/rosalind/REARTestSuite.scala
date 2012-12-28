package rosalind

import org.scalatest.FunSuite
import rosalind.REAR._
import scala.collection.mutable.Stack
 
class REARTestSuite extends FunSuite {
  
  def IntList(data:String) = data.split(" ").map(Integer.parseInt(_)).toList
    
  test("inverse permutation") {
	  expect(IntList("3 1 2 5 4")) { inversePermutation(IntList("2 3 1 5 4")) }
  }
  
  test("apply permutation") {
	  expect(IntList("4 1 5 3 2")) { applyPermutation(IntList("5 2 4 1 3"), IntList("3 1 2 5 4")) }
  }
  
  test("get breakpoints") {
    expect(List()) { breakpoints(IntList("1 2 3 4 5")) }
    expect(List((0,1), (2,3), (3,4), (6,7))) { breakpoints(IntList("2 3 1 6 5 4")) }
  }
    
  test("get breakpoint combos") {
    val bps = breakpoints(IntList("2 3 1 6 5 4"))
    expect(List((1,2),(1,3),(1,6),(3,6),(4,6))) { breakpointCombos(bps) }
  }
  
  test("test for identity both positive and negative") {
    expect(false) { isIdentity(IntList("5 2 3 1 4")) }
    expect(false) { isIdentity(IntList("5 4 3 2 1")) }
    expect(true) { isIdentity(IntList("1 2 3 4 5")) }
  }
  
  test("reversal distance with shortened data") {
    val start = IntList("1 2 3 4 5")
    val end = IntList("3 1 5 2 4")
	expect(4) { getNumReversals(start, end) }
  }

  test("reversal distance with data (set #1) from problem") {
    val start = IntList("1 2 3 4 5 6 7 8 9 10")
    val end = IntList("3 1 5 2 7 4 9 6 10 8")
	expect(9) { getNumReversals(start, end) }
  }
  test("reversal distance with data (set #2) from problem") {
    val start = IntList("3 10 8 2 5 4 7 1 6 9")
    val end = IntList("5 2 3 1 7 4 10 8 6 9")
	expect(4) { getNumReversals(start, end) }
  }

  test("reversal distance with data (set #3) from problem") {
    val start = IntList("8 6 7 9 4 1 3 10 2 5")
    val end = IntList("8 2 7 6 9 1 5 3 10 4")
	expect(5) { getNumReversals(start, end) }
  }

  test("reversal distance with data (set #4) from problem") {
    val start = IntList("3 9 10 4 1 8 6 7 5 2")
    val end = IntList("2 9 8 5 1 7 3 4 6 10")
	expect(7) { getNumReversals(start, end) }
  }

  test("reversal distance with data (set #5) from problem") {
    val start = IntList("1 2 3 4 5 6 7 8 9 10")
    val end = IntList("1 2 3 4 5 6 7 8 9 10")
	expect(0) { getNumReversals(start, end) }
  }

  test("reversal distances with real data") {
	  val data = """7 8 5 6 2 10 9 4 1 3
4 8 9 3 6 7 10 1 5 2

1 3 8 10 4 9 6 5 2 7
6 3 9 1 10 2 4 7 8 5

3 4 9 8 1 7 10 2 5 6
7 6 2 9 5 3 8 4 10 1

5 9 3 8 2 4 1 10 6 7
6 8 9 5 7 2 1 4 3 10

3 2 5 4 9 1 6 8 7 10
5 3 9 2 6 4 7 1 10 8""".split("\r\n").toList
    
	  (data grouped 3) foreach {
	    case first::second::whatever => print( getNumReversals(IntList(first), IntList(second)) + " " )
	  }
  }
}