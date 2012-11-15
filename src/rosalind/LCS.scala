package rosalind

import Stream._
import scala.Int

object LCS extends App with RosalindProblem {
	def getMinLength(strings:List[String]):Int =
	  //(strings map (w => w.length)).min
	  strings.foldLeft(Int.MaxValue)((minLength, word) => Math.min(minLength, word.length))
	  
	def getCommonSubstrings(strings:List[String]):Stream[String] = strings match {
	  case Nil => empty
	  case one::Nil => empty
	  case one::two::Nil => {
	    for (length <- (1 to getMinLength(strings)).reverse.toStream;
	    		sub <- one.toList.sliding(length)) yield sub.mkString
	  }
	  case first::second::rest => {
	    for (commonSub <- getCommonSubstrings(List(first,second));
	    		if (rest.forall(word => word.indexOf(commonSub) > -1))) yield commonSub
	  }
	}
  
	val words = getInput("LCS.input")
	
	getCommonSubstrings(words.take(2)).foreach(println)
	
	println("done")
	
	getCommonSubstrings(words) match {
	  case Stream() => println("None found")
	  case sub #:: more => println(sub)
	}
}