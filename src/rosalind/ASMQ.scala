package rosalind

object ASMQ {
  def nStatistics(dna:List[String]):(Int => Int) = {
    val allLengths = for (s <- dna) yield s.length
    val totalSize = allLengths.sum
    val lengthValues = (allLengths groupBy(identity)).map(a => (a._1, a._2.sum))
    val revLengths = lengthValues.keys.toSeq.sorted(Ordering.Int.reverse).toList

    def buildLengthSeq(aboveCount:Int, remLengths:List[Int]):List[(Int, Int)] = remLengths match {
      case Nil => Nil
      case next::rest => {
        val thisCount = aboveCount + lengthValues(next)
        (next, thisCount * 100 / totalSize)::buildLengthSeq(thisCount, rest)
      } 
    }
    val aboveLengthPercents = buildLengthSeq(0, revLengths)
    
    (nVal:Int) => aboveLengthPercents.dropWhile(_._2 < nVal).head._1
  }
}