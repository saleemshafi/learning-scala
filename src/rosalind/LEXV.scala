package rosalind


object LEXV extends App with RosalindProblem {
	
	getInput("LEXV.input").grouped(2).foreach(_ match {
	  case Nil => println("Invalid args")
	  case a::Nil => println("Invalid args")
	  case letters::length::Nil => {
	      val alphabet = letters.split(" ").mkString
	      val perm = ((1 to length.toInt) flatMap (i => LEXF.permutations(alphabet.toSet)(i))).toList
	      perm.sort(LEXF.lexSort(alphabet.toList)).foreach(word => println(word.mkString))
	  }
	})
	
}