package rosalind

trait RosalindProblem {
  def ??? = throw new Error("unimplemented")

  def getInput(filename: String) = 
    io.Source.fromFile(filename).mkString.split("\r\n").toList

  def runWithInput(filename: String)(func: (String) => Any) = {
    getInput(filename).foreach(
      dna => println(func(dna)))
  }

   val codon_table = Map(
    "UUU" -> 'F',    "CUU" -> 'L',    "AUU" -> 'I',    "GUU" -> 'V',
    "UUC" -> 'F',    "CUC" -> 'L',    "AUC" -> 'I',    "GUC" -> 'V',
    "UUA" -> 'L',    "CUA" -> 'L',    "AUA" -> 'I',    "GUA" -> 'V',
    "UUG" -> 'L',    "CUG" -> 'L',    "AUG" -> 'M',    "GUG" -> 'V',
    "UCU" -> 'S',    "CCU" -> 'P',    "ACU" -> 'T',    "GCU" -> 'A',
    "UCC" -> 'S',    "CCC" -> 'P',    "ACC" -> 'T',    "GCC" -> 'A',
    "UCA" -> 'S',    "CCA" -> 'P',    "ACA" -> 'T',    "GCA" -> 'A',
    "UCG" -> 'S',    "CCG" -> 'P',    "ACG" -> 'T',    "GCG" -> 'A',
    "UAU" -> 'Y',    "CAU" -> 'H',    "AAU" -> 'N',    "GAU" -> 'D',
    "UAC" -> 'Y',    "CAC" -> 'H',    "AAC" -> 'N',    "GAC" -> 'D',
    "UAA" -> '.',	 "CAA" -> 'Q',    "AAA" -> 'K',    "GAA" -> 'E',
    "UAG" -> '.',    "CAG" -> 'Q',    "AAG" -> 'K',    "GAG" -> 'E',
    "UGU" -> 'C',    "CGU" -> 'R',    "AGU" -> 'S',    "GGU" -> 'G',
    "UGC" -> 'C',    "CGC" -> 'R',    "AGC" -> 'S',    "GGC" -> 'G',
    "UGA" -> '.',    "CGA" -> 'R',    "AGA" -> 'R',    "GGA" -> 'G',
    "UGG" -> 'W',    "CGG" -> 'R',    "AGG" -> 'R',    "GGG" -> 'G') withDefaultValue '?'

   val start_codon = "AUG"
   val start_dna_codon = "ATG"
   val stop_codons = List("UAA", "UAG", "UGA")
   val stop_dna_codons = List("TAA", "TAG", "TGA")
    
   val dna_codon_table = (codon_table map (entry => (entry._1.replace("U", "T"), entry._2))) withDefaultValue '?'
}