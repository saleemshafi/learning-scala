package rosalind

import java.math.RoundingMode

class FixedDecimal(bigDecimal: java.math.BigDecimal) extends BigDecimal(bigDecimal) {
  def this(s: String) = this(new java.math.BigDecimal(s).setScale(5, RoundingMode.HALF_UP))

  def toFixedDecimal = this
  override def equals(that: Any) = that match {
    case t: FixedDecimal => this.toDouble == t.toDouble
    case _ => false
  }
}

object FixedDecimal {
  implicit def doubleToFixedDecimal(d: Double) = new FixedDecimal(d.toString())
  implicit def bigDecimalToFixedDecimal(bd: BigDecimal) = new FixedDecimal(bd.toString())
  implicit def stringToFixedDecimal(s: String) = new FixedDecimal(s)

  implicit object FixedDecimalOrdering extends Ordering[FixedDecimal] {
    def compare(x: FixedDecimal, y: FixedDecimal) = x.compare(y)
  }
}

