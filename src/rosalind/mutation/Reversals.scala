package rosalind.mutation

import scala._
import scala.annotation.tailrec

object Reversals {
  type Permutation = List[Int]
  type Breakpoint = (Int, Int)

  def reverseInterval(orig: Permutation, interval: (Int, Int)): Permutation = interval match {
    case (start, end) => (orig.view.take(start) ++ orig.view(start, end + 1).reverse ++ orig.view(end + 1, orig.length)).toList
  }

  def inversePermutationOf(orig: Permutation) =
    ((orig zipWithIndex) sortBy (_._1)) map (_._2 + 1)

  def applyPermutation(orig: Permutation, perm: Permutation) =
    orig map (i => perm(i - 1))

  def breakpoints(p: Permutation): List[Breakpoint] =
    (for (pair <- ((0 :: (p :+ (p.length + 1)) zipWithIndex) sliding 2); if (Math.abs(pair(0)._1 - pair(1)._1) != 1))
      yield (pair(0)._2, pair(1)._2)).toList

  def breakpointCombos(bps: List[Breakpoint]): List[(Int, Int)] = bps match {
    case Nil => Nil
    case _ =>
      (for (first <- (bps.init map (_._2)); second <- (bps.tail map (_._1)); if (first < second))
        yield (first, second)).toList
  }

  def getReversals(sigma: Permutation, tau: Permutation): List[(Int, Int)] = {

    @tailrec
    def getIdentityReversals(perms: List[(Permutation, List[(Int, Int)], List[Breakpoint])]): List[(Int, Int)] =
      perms match {
        case Nil => Nil
        case (perm, reversals, Nil) :: anything => reversals
        case _ => {
          val allRevsPlusBPs = (for ((pi, prevRevs, bps) <- perms; (start, end) <- breakpointCombos(bps); r = reverseInterval(pi, (start - 1, end - 1)))
            yield (r, prevRevs :+ (start, end), breakpoints(r)))
          val minLength = (allRevsPlusBPs minBy (a => a._3.length))._3.length
          val candidates = (for (rbp <- allRevsPlusBPs; if (rbp._3.length == minLength))
            yield (rbp._1, rbp._2, rbp._3))
          getIdentityReversals(candidates.removeDuplicates)
        }
      }

    val pi = applyPermutation(sigma, inversePermutationOf(tau))
    getIdentityReversals(List((pi, Nil, breakpoints(pi))))
  }

  def reversalDistance(sigma: Permutation, tau: Permutation) =
    getReversals(sigma, tau).length

}