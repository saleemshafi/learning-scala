package rosalind

import scala.math.BigDecimal.RoundingMode

object IPRB extends App with RosalindProblem {
	def chanceOfDominantPhenotype(dominant:Int, mixed:Int, recessive:Int):BigDecimal = {
		val total = Double.box(dominant + mixed + recessive)
		val side1 = dominant/total
		val side2 = (mixed/total)*(1/2d)*(1 + (dominant/(total-1)) + (1/2d)*((mixed-1)/(total-1)))
		val side3 = (recessive/total)*(dominant/(total-1)) + (recessive/total)*(mixed/(total-1))*(1/2d)
		BigDecimal.apply(side1 + side2 + side3).setScale(5, RoundingMode.HALF_UP)
	}
}