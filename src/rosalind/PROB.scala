package rosalind

object PROB extends App with RosalindProblem {
	def square(x:Double) = x * x
  
	def sameTwoProbability(prob:Double):Double = 
	  2*(square(prob / 2) + square((1-prob)/2))
  
	runWithInput("PROB.input")(
	    (s:String) => (for {gcprob <- s.split(" ")} 
	    					yield sameTwoProbability(gcprob toDouble)).mkString(" ")
	)
}