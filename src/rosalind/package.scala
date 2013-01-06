package object rosalind {
  def maxOccurring[T](ms: Seq[T]): (T, Int) =
    (ms.groupBy(identity) map { case (item, list) => (item -> list.length) }) maxBy (item => item._2)
}