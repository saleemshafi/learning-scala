package rosalind

import org.scalatest.FunSuite
import rosalind.mutation._
import rosalind.mutation.Reversals._

class MutationTestSuite extends FunSuite {

  def IntList(data: String) = data.split(" ").map(Integer.parseInt(_)).toList

  test("PERM: generate permutations of 1,2,3") {
    val perm3 = perms(1 to 3)
    expect(6) { perm3.length }
    assert(perm3.contains(List(1, 2, 3)))
    assert(perm3.contains(List(2, 1, 3)))
    assert(perm3.contains(List(2, 3, 1)))
    assert(perm3.contains(List(1, 3, 2)))
    assert(perm3.contains(List(3, 1, 2)))
    assert(perm3.contains(List(3, 2, 1)))
  }

  test("PERM: generate permutations of 1-4") {
    val perm4 = perms(1 to 4)
    expect(24) { perm4.length }
    assert(perm4.contains(List(1, 2, 3, 4)))
    assert(perm4.contains(List(1, 2, 4, 3)))
    assert(perm4.contains(List(1, 3, 2, 4)))
    assert(perm4.contains(List(1, 3, 4, 2)))
    assert(perm4.contains(List(1, 4, 2, 3)))
    assert(perm4.contains(List(1, 4, 3, 2)))
    assert(perm4.contains(List(2, 1, 3, 4)))
    assert(perm4.contains(List(2, 1, 4, 3)))
    assert(perm4.contains(List(2, 3, 1, 4)))
    assert(perm4.contains(List(2, 3, 4, 1)))
    assert(perm4.contains(List(2, 4, 1, 3)))
    assert(perm4.contains(List(2, 4, 3, 1)))
    assert(perm4.contains(List(3, 1, 2, 4)))
    assert(perm4.contains(List(3, 1, 4, 2)))
    assert(perm4.contains(List(3, 2, 1, 4)))
    assert(perm4.contains(List(3, 2, 4, 1)))
    assert(perm4.contains(List(3, 4, 1, 2)))
    assert(perm4.contains(List(3, 4, 2, 1)))
    assert(perm4.contains(List(4, 1, 2, 3)))
    assert(perm4.contains(List(4, 1, 3, 2)))
    assert(perm4.contains(List(4, 2, 1, 3)))
    assert(perm4.contains(List(4, 2, 3, 1)))
    assert(perm4.contains(List(4, 3, 1, 2)))
    assert(perm4.contains(List(4, 3, 2, 1)))
  }

  test("PERM: generate permutations of 1-7") {
    val perm7 = perms(1 to 7)
    expect(5040) { perm7.length }
    //    println(perm7.length)
    //    perm7 foreach (p => println(p mkString " "))
  }

  test("REAR: inverse permutation") {
    expect(IntList("3 1 2 5 4")) { inversePermutationOf(IntList("2 3 1 5 4")) }
  }

  test("REAR: apply permutation") {
    expect(IntList("4 1 5 3 2")) { applyPermutation(IntList("5 2 4 1 3"), IntList("3 1 2 5 4")) }
  }

  test("REAR: get breakpoints") {
    expect(List()) { breakpoints(IntList("1 2 3 4 5")) }
    expect(List((0, 1), (2, 3), (3, 4), (6, 7))) { breakpoints(IntList("2 3 1 6 5 4")) }
  }

  test("REAR: get breakpoint combos") {
    val bps = breakpoints(IntList("2 3 1 6 5 4"))
    expect(List((1, 2), (1, 3), (1, 6), (3, 6), (4, 6))) { breakpointCombos(bps) }
  }

  test("REAR: reversal distance with shortened data") {
    expect(4) { reversalDistance(IntList("1 2 3 4 5"), IntList("3 1 5 2 4")) }
  }

  test("REAR: reversal distance with data (set #1) from problem") {
    expect(9) { reversalDistance(IntList("1 2 3 4 5 6 7 8 9 10"), IntList("3 1 5 2 7 4 9 6 10 8")) }
  }
  test("REAR: reversal distance with data (set #2) from problem") {
    expect(4) { reversalDistance(IntList("3 10 8 2 5 4 7 1 6 9"), IntList("5 2 3 1 7 4 10 8 6 9")) }
  }

  test("REAR: reversal distance with data (set #3) from problem") {
    expect(5) { reversalDistance(IntList("8 6 7 9 4 1 3 10 2 5"), IntList("8 2 7 6 9 1 5 3 10 4")) }
  }

  test("REAR: reversal distance with data (set #4) from problem") {
    expect(7) { reversalDistance(IntList("3 9 10 4 1 8 6 7 5 2"), IntList("2 9 8 5 1 7 3 4 6 10")) }
  }

  test("REAR: reversal distance with data (set #5) from problem") {
    expect(0) { reversalDistance(IntList("1 2 3 4 5 6 7 8 9 10"), IntList("1 2 3 4 5 6 7 8 9 10")) }
  }

  test("REAR: reversal distances with real data") {
    expect(7) { reversalDistance(IntList("7 8 5 6 2 10 9 4 1 3"), IntList("4 8 9 3 6 7 10 1 5 2")) }
    expect(7) { reversalDistance(IntList("1 3 8 10 4 9 6 5 2 7"), IntList("6 3 9 1 10 2 4 7 8 5")) }
    expect(7) { reversalDistance(IntList("3 4 9 8 1 7 10 2 5 6"), IntList("7 6 2 9 5 3 8 4 10 1")) }
    expect(6) { reversalDistance(IntList("5 9 3 8 2 4 1 10 6 7"), IntList("6 8 9 5 7 2 1 4 3 10")) }
    expect(9) { reversalDistance(IntList("3 2 5 4 9 1 6 8 7 10"), IntList("5 3 9 2 6 4 7 1 10 8")) }
  }

  test("SIGN") {
    val permSigned = signedPermutations((1 to 6).toList)
    expect(46080) { permSigned.length }
    //    println(permSigned.length)
    //    permSigned foreach (p => println(p mkString " "))
  }

  test("PPER: permutations with data from problem") {
    expect(3360) { numPermutations(16, 3, 1000000) }
    expect(108800) { numPermutations(87, 10, 1000000) }
  }

  test("PPER: permutations with real data") {
    expect(438400) { numPermutations(85, 8, 1000000) }
  }

  test("sorting by reversals with data from problem") {
    val start = IntList("1 2 3 4 5 6 7 8 9 10")
    val end = IntList("1 8 9 3 2 7 6 5 4 10")
    val reversals = getReversals(start, end)
    expect(2) { reversals.length }
    expect((4, 9)) { reversals.head }
    expect((2, 5)) { reversals.last }
  }

  test("sorting by reversals with real data") {
    val reversals = getReversals(IntList("2 5 9 6 4 7 1 8 10 3"), IntList("3 6 2 9 7 5 8 4 10 1"))
    expect(8) { reversals.length }
    expect((3, 8)) { reversals(0) }
    expect((7, 9)) { reversals(1) }
    expect((5, 7)) { reversals(2) }
    expect((1, 8)) { reversals(3) }
    expect((1, 7)) { reversals(4) }
    expect((1, 5)) { reversals(5) }
    expect((1, 3)) { reversals(6) }
    expect((1, 10)) { reversals(7) }
  }
}