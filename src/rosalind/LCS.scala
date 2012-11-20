package rosalind

import Stream._
import scala.Int

object LCS extends App with RosalindProblem {
	def getMinLength(strings:List[String]):Int =
	  //(strings map (w => w.length)).min       // this version is easier to read, but i think its slower
	  strings.foldLeft(Int.MaxValue)((minLength, word) => Math.min(minLength, word.length))
	  
	def getCommonSubstrings(one:String, two:String):Stream[String] = 
	    for (length <- (1 to getMinLength(List(one,two))).reverse.toStream;
	    		sub <- one.toList.sliding(length); if (two.indexOf(sub.mkString) > -1)) yield sub.mkString
	
	def filterSubstrings(substrings:Stream[String], superString:String):Stream[String] = 
	  substrings.filter(sub => superString.contains(sub))
	
	def getCommonSubstrings(strings:List[String]):Stream[String] = strings match {
	  case Nil | _::Nil => empty
	  case one::two::Nil => getCommonSubstrings(one, two)
	  case first::second::rest => {
	    rest.foldLeft(getCommonSubstrings(first,second) ) {
	      (substrings:Stream[String], nextString:String) => filterSubstrings(substrings, nextString)
	    }
	  }
	}
  
	val words = getInput("LCS.input")
	
//	getCommonSubstrings(words.take(2)).take(50).foreach(println)
		

  	getCommonSubstrings(words) match {
	  case Stream() => println("None found")
	  case sub #:: more => println(sub)
	}
}