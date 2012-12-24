package rosalind

import org.scalatest.FunSuite
import rosalind.MPRT._
import scala.collection.mutable.Stack
 
class MPRTTestSuite extends FunSuite {
  test("finding motifs with overlapping matches") {
    val proteinIDs = """P02725_GLP_PIG""".split("\r\n").toList

	val pattern = "N[^P][ST][^P]".r //  N{P}[ST]{P}
	
	val occurs = findOccurrences(pattern) _
	expect(List(16, 19, 39)) { occurs("P02725_GLP_PIG").toList }
  }
  
  test("finding motifs with data from problem") {
    val proteinIDs = """A2Z669
B5ZC00
P20840_SAG1_YEAST""".split("\r\n").toList

	val pattern = "N[^P][ST][^P]".r //  N{P}[ST]{P}
	val occurs = findOccurrences(pattern) _
	expect(Nil) { occurs("A2Z669").toList }
	expect(List(85, 118, 142, 306, 395)) { occurs("B5ZC00").toList }
	expect(List(79, 109, 135, 248, 306, 348, 364, 402, 485, 501, 614)) { occurs("P20840_SAG1_YEAST").toList }
  }

  test("finding motifs with real data") {
    val proteinIDs = """Q9QSP4
Q8ER84
P02748_CO9_HUMAN
Q67JS9
Q1JHI2
P01045_KNH2_BOVIN
P01588_EPO_HUMAN
P02765_A2HS_HUMAN
B4U0J5
P37803
A8GP89
P27918_PROP_HUMAN
P81428_FA10_TROCA""".split("\r\n").toList

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