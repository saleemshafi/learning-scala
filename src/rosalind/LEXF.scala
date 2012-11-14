package rosalind

object LEXF extends App with RosalindProblem {
	def permutations[T](alphabet: Set[T])(length: Int):List[List[T]] = 
	  if (length == 0) List(List())
	  else for (letter <- alphabet.toList; prevWords <- permutations(alphabet)(length - 1))
	    yield letter::prevWords
	    
	def lexSort[T](alphabet: List[T]) = {
	    def sorted(a:List[T], b:List[T]):Boolean = a match {
	      case List() => true
	      case aHead::aTail => {
	    	  val aIndex = alphabet.indexOf(aHead)
	    	  val bIndex = alphabet.indexOf(b.head)
	    	  if (aIndex < bIndex) true
	    	  else if (bIndex < aIndex) false
	    	  else sorted(aTail, b.tail)
	      }
	    }
	    
	    sorted _
	}
  
	getInput("LEXF.input").grouped(2).foreach(_ match {
	  case Nil => println("Invalid args")
	  case a::Nil => println("Invalid args")
	  case letters::length::Nil => {
	      val alphabet = letters.split(" ").mkString
	      permutations(alphabet.toSet)(length.toInt).sort(lexSort(alphabet.toList)).foreach(word => println(word.mkString))
	  }
	})
	
}