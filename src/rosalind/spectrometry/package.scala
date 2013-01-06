package rosalind

import rosalind.FixedDecimal._

package object spectrometry {
  def monoisotopicMassOf = Map(
    'A' -> 71.03711,
    'C' -> 103.00919,
    'D' -> 115.02694,
    'E' -> 129.04259,
    'F' -> 147.06841,
    'G' -> 57.02146,
    'H' -> 137.05891,
    'I' -> 113.08406,
    'K' -> 128.09496,
    'L' -> 113.08406,
    'M' -> 131.04049,
    'N' -> 114.04293,
    'P' -> 97.05276,
    'Q' -> 128.05858,
    'R' -> 156.10111,
    'S' -> 87.03203,
    'T' -> 101.04768,
    'V' -> 99.06841,
    'W' -> 186.07931,
    'Y' -> 163.06333)

  def aminoAcidWith = monoisotopicMassOf.map { case (acid, mass) => (mass.toFixedDecimal -> acid) }

  def aminoAcidsCloseTo(tolerance: Double)(approxMass: FixedDecimal): Iterable[Char] =
    for ((acid, mass) <- monoisotopicMassOf; if ((approxMass - mass).abs < tolerance)) yield acid

  type Spectrum = Seq[FixedDecimal]

  def minkowskiDifference(s1: Spectrum, s2: Spectrum): Seq[FixedDecimal] =
    for (item1 <- s1; item2 <- s2) yield (item1 - item2).toFixedDecimal

  def minkowskiDifferenceMap(s1: Spectrum, s2: Spectrum): Map[BigDecimal, Int] =
    minkowskiDifference(s1, s2).groupBy(identity) map { case (item, list) => (item -> list.length) }

}