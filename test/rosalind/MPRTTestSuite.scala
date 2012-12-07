package rosalind

import org.scalatest.FunSuite
import rosalind.MPRT._
import scala.collection.mutable.Stack
 
class MPRTTestSuite extends FunSuite {
  test("finding motifs with data from problem") {
    val proteinIDs = """A2Z669
B5ZC00
P20840_SAG1_YEAST""".split("\r\n").toList

	val pattern = "N[^P][ST][^P]".r //  N{P}[ST]{P}
	expect(Nil) { findOccurrences(pattern)("A2Z669").toList }
	expect(List(85, 118, 142, 306, 395)) { findOccurrences(pattern)("B5ZC00").toList }
	expect(List(79, 109, 135, 248, 306, 348, 364, 402, 485, 501, 614)) { findOccurrences(pattern)("P20840_SAG1_YEAST").toList }
  }

  test("finding motifs with real data") {
    val proteinIDs = """A2Z669
B5ZC00
P20840_SAG1_YEAST""".split("\r\n").toList

	val pattern = "N[^P][ST][^P]".r //  N{P}[ST]{P}
	
	proteinIDs.foreach(p => findOccurrences(pattern)(p) match {
	  case Nil => 
	  case matches => {
	    println(p)
	    println(matches.mkString(" "))
	  }
	})
  }
}