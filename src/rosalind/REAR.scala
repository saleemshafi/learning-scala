package rosalind

import scala._

object REAR extends App with RosalindProblem {
  def stripCommonPrefix[T](a:List[T], b:List[T]):(List[T], List[T]) = (a, b) match {
    case (Nil, Nil) => (Nil, Nil)
    case (aFirst::aRest, bFirst::bRest) => 
	    if (aFirst == bFirst) stripCommonPrefix(aRest, bRest)
	    else (a, b)
  }

  def reverse[T](orig:List[T], elem: T):List[T] = {
    val (beginning, end) = orig.splitAt(orig.indexOf(elem)+1)
    beginning.reverse ++ end
  } 

  def getNumReversals[T](start:List[T], end:List[T], permsSoFar:Int):Int = 
    stripCommonPrefix(start, end) match {
    	case (Nil, Nil) => permsSoFar
    	case (source, target) => {
    	  Math.min(
    			  getNumReversals(reverse(source, target.head), target, permsSoFar+1),
    			  getNumReversals(reverse(source.reverse, target.last), target.reverse, permsSoFar+1))    			  
    	}
  	} 
  
  val data = getInput("REAR.input").grouped(3)
  val answers = for (List(start, end, space) <- data)
    yield getNumReversals(start.split(" ").toList, end.split(" ").toList, 0) 
  println(answers.mkString(" "))
}