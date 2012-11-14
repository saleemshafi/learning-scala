package rosalind

object PROT extends App with RosalindProblem {
	def encode(rna: String): String = (for (codon <- rna.grouped(3)) yield codon_table(codon)).mkString

	runWithInput("PROT.input")(encode)
}