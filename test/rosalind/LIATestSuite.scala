package rosalind

import org.scalatest.FunSuite

import rosalind.LIA._
import scala.math.BigDecimal.RoundingMode

class LIATestSuite extends FunSuite with RosalindProblem {
  
  test("mate") {
	  expect((BigDecimal.apply("0.250"), BigDecimal.apply("0.500"), BigDecimal.apply("0.250"))) { Genotype(true, false).mate(Genotype(true, false)) }
	  expect((BigDecimal.apply("0.500"), BigDecimal.apply("0.500"), BigDecimal.apply("0.000"))) { Genotype(true, true).mate(Genotype(true, false)) }
  }
  
  test("independent alleles with data from problem") {
    val generations = 2
    val numOrgs = 1
    
    expect(BigDecimal.apply("0.684")) { percentLiklihood(generations)(numOrgs).setScale(3, RoundingMode.HALF_UP) }
  }
  
  test("independent alleles with real data") {
    val generations = 7
    val numOrgs = 34
    
    expect(BigDecimal.apply("0.3739")) { percentLiklihood(generations)(numOrgs).setScale(4, RoundingMode.HALF_UP) }
  }
  
}