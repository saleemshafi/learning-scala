package rosalind

import org.scalatest.FunSuite
import rosalind.PCOV._
import scala.collection.mutable.Stack
 
class PCOVTestSuite extends FunSuite with RosalindProblem {
  test("cyclic chromosomes with data from problem") {
    val kmers = """ATTAC
TACAG
GATTA
ACAGA
CAGAT
TTACA
AGATT""".split("\r\n").toList

	val cs = getCyclicSuperstr(kmers)
	expect(7) { cs.length }
	assert((cs+cs).indexOf("GATTACA") > -1)
  }

  test("transitions and transversions with real data") {
    val kmers = getInput("PCOV.input")

	val cs = getCyclicSuperstr(kmers)
	println(cs)
	expect("AAACGCTTGTGATTCAGCAATATGAGCCAGATGTGCCAATATAACCCGTCGAGACCAGTGGGTGGCATCGTACCCCAGACTACAGATGTGCGACCCTTCAGTTCCTTGCGGATAGACTGAACAAGGGAACGGAAAGCCCAGGCGCGTCTGGGAGCTCAGATTGTTCTCATTCGAGATACTACAATAATGTGGTTTAGTCCACGATACATGCGCCGCCATACTCAAAGGGAACATCTGACGAGGAGGACACATAAGCGTTTCGGGCGTCCACAGCGCGGTTACGGGGGGGCCATTTATTCAGTTTTTTGTGACGCCTTGGTTGTACGGTTTCACGGTGACGGCACCGGAGCGGTAAAGGTTGGGGAAATACAGAGGGATTCGGGTCACCCATCATTATGACTTTTGCAAGTTTACTGGTACAAGAAACTAACGTGACCCAAAGACAATCGCTCTACTGGCTGGGCACGGCGGGCACTGCAGCGACAATTGGGCTACCCTTGAGTCCACAAAATGCCACCTGCCGCTATATTTAGTGATAAGTATCGTTTAAAAATCCTGTCCCCGATAGGAGTAGCACAATTTGTGCCGTTGTATTCCCAAATGAGCAGTTACTACAGGACGCTCGGAACGTGATTCTTTTATTGAATCACATGTTGGCAAAGCTGTGGCGATCAAGTAATGATATCGGAGGGGCAAGGATGGTGAGAGTGCTAAACGGTCTCGCCCAGTGGCTGTGCGCGCTCCTTACAACAACTCTTGTTACATTGACACCCTTCGCCACCGAACTAGAGATCAAGGTCGTTAAGGCATATGACTAAAGATTAAGACCTCTCTGGATCTAGGCCGTGTACGGCAAAACGGACTGGCTGTACCTACCCAGAGCGTCCACCGTATACTTGTCCTTGAGGGGTAACAGGTGTGGGTCTAGAATTAATATTAATGAGGGGCCCGCTTGTTTCCGAGACCTCCGAGTATGAGGCGTGTCCCTATTCGCGAAAGCAATCGATATTGTCTGAAGTTTTTCTACGCATGTCAAGGGGCGCGCCTGGGTTGTTTGGCCGTCAGGCCTCAATCTCCATAACAACGGAGTGGGCTTGTCAGTCGTACTGTGCGGAGGGACTCAAATCAGCCGGACGTCACTATAAACACAGTCAAGTTTGCCCGACGGACTGCTCTCCCCAGTCACGGGCGAAAGGTACATTTATGGATTATCCCGATCATCGCAGTGGGCCTGGGCCTGGACCGCACATCATCATATCTATAGGCCGGGTACGCCTAGCTAGTGAGACCCTACAAATCGACCGAAAGTTGGAACTGTTCCCTCGGTCGCGGGGCATATCAAACTACGCCAATATTATTCGCTTGTTTCTCTAGTTCACCGCACTTTCGACATCCGCATTAAACTGCGCATCCGAGGCAGTATTTGTAATTTACCATGGCACGTTTGTTCCCTAGTCGGGTCTTATTACGCAGCGGGTCCGAACGACCCTGACACTCAGGTCGTACCATTCCGTGCCGTAGGAAGCGGGGTGTGCTACATATTGCCCTCCTAGAATTTTCTAGTTGGATTTTAATGCTAGAAGGAGTCTATGCAACTTTGCTGAGGAATGTGCCAAGTCACAGACCCACCGTAGATTTCTGGCTCCTGAATAGAATGGTACAGGATAAGAAGTAGGTGGTCTAAACCGGTAGGCGAAAGACCTCTCTTCTATGACCTGTCACGACCATCAGGGCGAAAGGAAGCTTACTCTGGATGACTCGCGACAAGGGTTGAGCCACCAACGGCTAGGTGAAGGAAGAGCTGTTGGCATCTACCGATAAGAGCTCTCAGATAGTCTCCTTTTTTAACCGTCCACGCTTGCACGAGGCAGGTACAGGCCACAATCCTCTTTCGGCGCTCTTGTCACCCCGGAGTAAGTTAGTCTGACCGTATCGTTCCTTATCTCCTATGGGCTGGTAGGGGCTAGTATGCTCTCGGTACGCACCCCCCGGCGATGACC".length) { cs.length }
	assert((cs+cs).indexOf("AAACGCTTGTGATTCAGCAATATGAGCCAGATGTGCCAATATAACCCGTCGAGACCAGTGGGTGGCATCGTACCCCAGACTACAGATGTGCGACCCTTCAGTTCCTTGCGGATAGACTGAACAAGGGAACGGAAAGCCCAGGCGCGTCTGGGAGCTCAGATTGTTCTCATTCGAGATACTACAATAATGTGGTTTAGTCCACGATACATGCGCCGCCATACTCAAAGGGAACATCTGACGAGGAGGACACATAAGCGTTTCGGGCGTCCACAGCGCGGTTACGGGGGGGCCATTTATTCAGTTTTTTGTGACGCCTTGGTTGTACGGTTTCACGGTGACGGCACCGGAGCGGTAAAGGTTGGGGAAATACAGAGGGATTCGGGTCACCCATCATTATGACTTTTGCAAGTTTACTGGTACAAGAAACTAACGTGACCCAAAGACAATCGCTCTACTGGCTGGGCACGGCGGGCACTGCAGCGACAATTGGGCTACCCTTGAGTCCACAAAATGCCACCTGCCGCTATATTTAGTGATAAGTATCGTTTAAAAATCCTGTCCCCGATAGGAGTAGCACAATTTGTGCCGTTGTATTCCCAAATGAGCAGTTACTACAGGACGCTCGGAACGTGATTCTTTTATTGAATCACATGTTGGCAAAGCTGTGGCGATCAAGTAATGATATCGGAGGGGCAAGGATGGTGAGAGTGCTAAACGGTCTCGCCCAGTGGCTGTGCGCGCTCCTTACAACAACTCTTGTTACATTGACACCCTTCGCCACCGAACTAGAGATCAAGGTCGTTAAGGCATATGACTAAAGATTAAGACCTCTCTGGATCTAGGCCGTGTACGGCAAAACGGACTGGCTGTACCTACCCAGAGCGTCCACCGTATACTTGTCCTTGAGGGGTAACAGGTGTGGGTCTAGAATTAATATTAATGAGGGGCCCGCTTGTTTCCGAGACCTCCGAGTATGAGGCGTGTCCCTATTCGCGAAAGCAATCGATATTGTCTGAAGTTTTTCTACGCATGTCAAGGGGCGCGCCTGGGTTGTTTGGCCGTCAGGCCTCAATCTCCATAACAACGGAGTGGGCTTGTCAGTCGTACTGTGCGGAGGGACTCAAATCAGCCGGACGTCACTATAAACACAGTCAAGTTTGCCCGACGGACTGCTCTCCCCAGTCACGGGCGAAAGGTACATTTATGGATTATCCCGATCATCGCAGTGGGCCTGGGCCTGGACCGCACATCATCATATCTATAGGCCGGGTACGCCTAGCTAGTGAGACCCTACAAATCGACCGAAAGTTGGAACTGTTCCCTCGGTCGCGGGGCATATCAAACTACGCCAATATTATTCGCTTGTTTCTCTAGTTCACCGCACTTTCGACATCCGCATTAAACTGCGCATCCGAGGCAGTATTTGTAATTTACCATGGCACGTTTGTTCCCTAGTCGGGTCTTATTACGCAGCGGGTCCGAACGACCCTGACACTCAGGTCGTACCATTCCGTGCCGTAGGAAGCGGGGTGTGCTACATATTGCCCTCCTAGAATTTTCTAGTTGGATTTTAATGCTAGAAGGAGTCTATGCAACTTTGCTGAGGAATGTGCCAAGTCACAGACCCACCGTAGATTTCTGGCTCCTGAATAGAATGGTACAGGATAAGAAGTAGGTGGTCTAAACCGGTAGGCGAAAGACCTCTCTTCTATGACCTGTCACGACCATCAGGGCGAAAGGAAGCTTACTCTGGATGACTCGCGACAAGGGTTGAGCCACCAACGGCTAGGTGAAGGAAGAGCTGTTGGCATCTACCGATAAGAGCTCTCAGATAGTCTCCTTTTTTAACCGTCCACGCTTGCACGAGGCAGGTACAGGCCACAATCCTCTTTCGGCGCTCTTGTCACCCCGGAGTAAGTTAGTCTGACCGTATCGTTCCTTATCTCCTATGGGCTGGTAGGGGCTAGTATGCTCTCGGTACGCACCCCCCGGCGATGACC") > -1)
  }
}