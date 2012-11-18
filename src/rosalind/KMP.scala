package rosalind

import Stream._

object KMP extends App with RosalindProblem {

  def suffixMatchLength(s: String, k: Int, prevLength: Int): Stream[Int] = {
    if (k >= s.length) empty
    else prevLength match {
      case 0 => {
        val l = if (s.head == s(k)) 1 else 0
        l #:: suffixMatchLength(s, k + 1, l)
      }
      case _ => {
        val prefix = s.view(0, prevLength + 1).mkString
        val suffix = s.view(k - prevLength, k + 1).mkString 
        if (prefix.equals(suffix))
          (prevLength + 1) #:: suffixMatchLength(s, k + 1, prevLength + 1)
        else
          suffixMatchLength(s, k, prevLength - 1)
      }
    }
  }
  
	def failureArray(dna: String): Stream[Int] = 0 #:: suffixMatchLength(dna, 1, 0)

	def failureArrayAsString(dna: String): String =
	  failureArray(dna).mkString(" ")
	  
	runWithInput("KMP.input")(failureArrayAsString)
}