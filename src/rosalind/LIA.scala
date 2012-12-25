package rosalind

object LIA extends App with RosalindProblem {
  
	def binomialProbability(trials:Int, events:Seq[Int], prob:BigDecimal):BigDecimal =
	  (for(e <- events) yield binomialProbability(trials, e, prob)).sum
  
	def binomialProbability(trials:Int, events:Int, prob:BigDecimal):BigDecimal = {
		BigDecimal.apply(ASPC.choose(trials, events)) * prob.pow(events) * (1-prob).pow(trials-events)
	}
  
	type GenotypeRates = (BigDecimal, BigDecimal, BigDecimal)
	case class Genotype(one:Boolean, two:Boolean) // AA = T,T Aa = T,F or F,T, aa = F,F
	{
		def mate(spouse:Genotype):GenotypeRates = spouse match {
		  case Genotype(true, true) => this match {
		  	case Genotype(true, true) => (1.000, 0.000, 0.000)
			case Genotype(true, false) | Genotype(false, true) => (0.500, 0.500, 0.00)
			case Genotype(false, false) => (0.000, 1.000, 0.000)
		  }
		  case Genotype(true, false) | Genotype(false, true) => this match {
		  	case Genotype(true, true) => (0.500, 0.500, 0.000)
			case Genotype(true, false) | Genotype(false, true) => (0.250, 0.500, 0.250)
			case Genotype(false, false) => (0.000, 0.500, 0.500)
		  }
		  case Genotype(false, false) => this match {
		  	case Genotype(true, true) => (0.000, 1.000, 0.000)
			case Genotype(true, false) | Genotype(false, true) => (0.000, 0.500, 0.500)
			case Genotype(false, false) => (0.000, 0.000, 1.000)
		  }
		}
	}
	
	
	def percentLiklihood(k:Int)(n:Int):BigDecimal = {
	  
	  def runRates(count:Int, prevGen:GenotypeRates, spouse:Genotype):GenotypeRates = 
	    if (count == 0) prevGen
	    else {
	      val first = spouse.mate(Genotype(true,true))
	      val second = spouse.mate(Genotype(true, false))
	      val third = spouse.mate(Genotype(false, false))
	      
	      val probFirst = (first._1 * prevGen._1) + (second._1 * prevGen._2) + (third._1 * prevGen._3)
	      val probSecond = (first._2 * prevGen._1) + (second._2 * prevGen._2) + (third._2 * prevGen._3)
	      val probThird = (first._3 * prevGen._1) + (second._3 * prevGen._2) + (third._3 * prevGen._3)
	      val newRates = (probFirst, probSecond, probThird) 
	      
	      runRates( count-1, newRates, spouse)
	    }
	  
	  val gen0 = (BigDecimal.apply(0.000), BigDecimal.apply(1.000), BigDecimal.apply(0.000))
	  val numOffspring = 1 << k
	  val nGenRates = runRates(k, gen0, Genotype(true, false))
	  
	  binomialProbability( numOffspring, n to numOffspring, nGenRates._2.pow(2) )
	}
}