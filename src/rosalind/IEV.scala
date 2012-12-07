package rosalind

object IEV {
  def numChildrenWithDomPhenotype(genoFamily:(Int, Int, Int, Int, Int, Int)):BigDecimal = {
    val domRate = (1, 1, 1, 0.75, 0.5, 0)
    val avgKids = 2
    ((genoFamily._1 * domRate._1) +
     (genoFamily._2 * domRate._2) +
     (genoFamily._3 * domRate._3) +
     (genoFamily._4 * domRate._4) +
     (genoFamily._5 * domRate._5) +
     (genoFamily._6 * domRate._6)) * avgKids
  }
}