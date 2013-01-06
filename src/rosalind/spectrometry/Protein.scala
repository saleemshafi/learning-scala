package rosalind.spectrometry

import scala.annotation.tailrec
import rosalind.FixedDecimal
import rosalind.FixedDecimal._

// Protein
class Protein(val p: String) {
  def mass =
    (p map (acid => monoisotopicMassOf(acid)) sum).toFixedDecimal

  def prefixes = for (i <- (1 to p.length - 1)) yield new Protein(p.view(0, i).mkString)
  def suffixes = for (i <- (1 to p.length - 2)) yield new Protein(p.view(i, p.length - 1).mkString)

  def completeSpectrum =
    for (segment <- (prefixes ++ suffixes)) yield segment.mass

  def toProtein = this
  def length = p.length
  override def toString = p
  override def equals(that: Any) = that match {
    case t: Protein => this.p.equals(t.p)
    case _ => false
  }
}

object Protein {
  implicit def stringToProtein(s: String): Protein = new Protein(s)

  /**
   * The spectrum must only contain b-ions.  The resulting protein
   * will have length len(spectrum)-1.
   */
  def fromSpectrum(spectrum: Spectrum): Protein = {
    def massDiff(masses: Seq[FixedDecimal]): FixedDecimal = masses.toList match {
      case a :: (b :: rest) => b - a
      case _ => 0
    }
    val protein = for (mass <- spectrum.sliding(2).map(massDiff))
      yield aminoAcidWith(mass)
    protein.mkString
  }

  /**
   * The spectrum should contain both b-ions and y-ions.  The resulting
   * protein will have length len(spectrum)/2 - 1
   */
  def fromFullSpectrum(spectrum: Seq[FixedDecimal]): Protein = {
    val n = (spectrum.length / 2) - 1

    @tailrec
    def findAminoAcid(s1: BigDecimal, masses: Spectrum): (Char, Spectrum) = {
      aminoAcidWith.get(masses.head - s1) match {
        case None => findAminoAcid(s1, masses.tail)
        case Some(ch) => (ch, masses)
      }
    }

    @tailrec
    def findAminoAcids(acids: Seq[Char], spectrum: Spectrum): Seq[Char] = {
      if (acids.length == n) acids
      else {
        val (ch, rest) = findAminoAcid(spectrum.head, spectrum.tail)
        findAminoAcids(acids :+ ch, rest)
      }
    }

    findAminoAcids(Nil, spectrum sorted).mkString
  }

  def inferFromMassSpectrum(spectrum: Spectrum): Protein = {
    type Edge = (FixedDecimal, FixedDecimal, Char)
    type SpectrumGraph = Map[FixedDecimal, Seq[Edge]]

    def getSpectrumGraph(spectrum: Spectrum): SpectrumGraph =
      (for (a <- spectrum; b <- spectrum; protein <- aminoAcidsCloseTo(0.01)(b - a))
        yield (a, b, protein)).groupBy(a => a._1)

    def getProteinStrings(graph: Map[FixedDecimal, Seq[Edge]], start: FixedDecimal): Seq[Protein] =
      graph.get(start) match {
        case None => List(new Protein(""))
        case Some(edges) => for (
          (x, node, ch) <- edges;
          suffix <- getProteinStrings(graph, node)
        ) yield new Protein(ch + suffix.toString)
      }

    val graph = getSpectrumGraph(spectrum)
    (for (start <- spectrum; str <- getProteinStrings(graph, start))
      yield str) maxBy (_ length)
  }

}
