package rosalind

import org.scalatest.FunSuite
import rosalind.PROB2._
import scala.collection.mutable.Stack
 
class PROB2TestSuite extends FunSuite {
  test("random strings with data from problem") {
    val s = "ACGATACAA"
    val A = "0.129 0.287 0.423 0.476 0.641 0.742 0.783".split(' ').map(a => a.toDouble)
    val expected = "-5.737 -5.217 -5.263 -5.360 -5.958 -6.628 -7.009".split(' ').map(a => BigDecimal.apply(a))

    A.zip(expected).foreach {
      pair => expect(pair._2) { logProbability(s)(pair._1) }
    }
  }

  test("random strings with real data") {
    val s = "CAGAAGCCGGGGGTAACTGGTTAGTGCCGCTGAGGATATAAACTCTTGATTCCTACGTGAGCACGACTTTGGCCACTAACGCAC"
    val A = "0.093 0.143 0.176 0.224 0.299 0.317 0.362 0.433 0.471 0.537 0.582 0.619 0.679 0.746 0.753 0.812 0.855 0.940".split(' ').map(a => a.toDouble)

    A.foreach {
      a => print(logProbability(s)(a)+" ")
    }
  }
}