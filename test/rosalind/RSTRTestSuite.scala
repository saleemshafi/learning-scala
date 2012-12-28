package rosalind

import org.scalatest.FunSuite

import rosalind.PROB2._
import rosalind.LIA._
import scala.math.BigDecimal.RoundingMode

class RSTRTestSuite extends FunSuite with RosalindProblem {

  def round(x:Double) = BigDecimal.apply(x).setScale(4, RoundingMode.HALF_UP)
  def probOfAtLeast1in(events:Int, prob:Double):Double =
    // 1 - binomialProbability(events, events, 1-prob)
    1 - Math.pow(1-prob, events)
  
  test("longest common subsequence with data from problem") {
    val target = "AACCTTGG"
    val nTrials = 90000
    val gc = 0.6
    expect( BigDecimal.apply("0.6885")) { 
      round(probOfAtLeast1in(nTrials, gcProbability(target)(gc)))
    }
  }
  
  test("longest common subsequence with real data") {
    val target = "AGGGTTTC"
    val nTrials = 90555
    val gc = 0.428100
    expect( BigDecimal.apply("0.7194")) { 
      round(probOfAtLeast1in(nTrials, gcProbability(target)(gc)))
    }
  }
}