package rosalind

package object mutation {
  def perms[T](s: Seq[T]) = s.toList.permutations.toSeq

  def numPermutations(n: Int, r: Int, mod: Int = Int.MaxValue): Int =
    // n!/[(n-r)!] = n * (n-1) * (n-2) * .. * (n-r+1)
    (n - r + 1 to n).reduceLeft((a: Int, b: Int) => ((a * b) % mod))

  def signedPermutations(elements: Seq[Int]): Seq[Seq[Int]] = elements match {
    case Nil => List(List())
    case h :: t => for (
      prevSeq <- signedPermutations(t);
      multiplier <- List(1, -1);
      index <- 0 to prevSeq.length
    ) yield prevSeq.patch(index, List(multiplier * h), 0)
  }

}