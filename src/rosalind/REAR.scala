package rosalind

import scala._
import scala.annotation.tailrec

object REAR extends App with RosalindProblem {
  def reverse[T](orig:List[T], start:Int, end:Int):List[T] =
    (orig.view.take(start) ++ orig.view(start, end+1).reverse ++ orig.view(end+1, orig.length)).toList
    
  def inversePermutation(orig:List[Int]):List[Int] = 
    ((orig zipWithIndex) sortBy (_._1)) map (_._2 + 1)
    
  def applyPermutation(orig:List[Int], perm:List[Int]) =
    orig map (i => perm(i-1))
  
  type Permutation = List[Int]
  type Breakpoint = (Int, Int)
  
  def breakpoints(p:Permutation):List[Breakpoint] = 
    (for (pair <- (( 0::(p:+(p.length+1)) zipWithIndex) sliding 2); if (Math.abs(pair(0)._1 - pair(1)._1) != 1)) 
    	yield (pair(0)._2, pair(1)._2)).toList
  
  def breakpointCombos(bps:List[Breakpoint]):List[(Int, Int)] = bps match {
    case Nil => Nil
    case _ =>
	    (for (first <- (bps.init map (_._2)); second <- (bps.tail map (_._1)); if (first < second))
	    	yield (first, second)).toList
  }
    
  def anyStrip(first:Int, last:Int) = true
  def increasingStrip(first:Int, last:Int) = first <= last
  def decreasingStrip(first:Int, last:Int) = first > last
  
  def strips(p:List[Int]):List[List[Int]] = strips(p, anyStrip)
  def strips(p:List[Int], filter:(Int, Int) => Boolean):List[List[Int]] =
    (for (pair <- (breakpoints(p) sliding 2); if (pair(0)._2 != pair(1)._1))
      yield (pair(0)._2 to pair(1)._1).toList).toList

  def isIdentity(pi:Permutation) = {
    val pwi = pi zipWithIndex
    val l = (for ((v, i) <- pwi; if (v != i+1)) yield v)
    l.isEmpty
  }

  def numReversalsForSort(orig:Permutation):Int = orig match {
	    case Nil => 0
	    case _ => if (orig.last == orig.length) numReversalsForSort(orig.init) 
	    		  else {
	    		    val i = orig.indexOf(orig.length)
	    		    1 + numReversalsForSort(reverse(orig, i, orig.length-1))
	    		  }
    }

  def getNumReversals(sigma:Permutation, tau:Permutation):Int = {
    
    @tailrec
    def getNumReversals(revCount:Int, perms:List[(Permutation, List[Breakpoint])]):Int = perms match {
      case Nil => -1
      case (perm, Nil)::anything => revCount
      case _ => {
      	val allRevsPlusBPs = (for ((pi, bps) <- perms; (start, end) <- breakpointCombos(bps); r = reverse(pi, start-1, end-1)) 
      	  yield (r, breakpoints(r)))
      	val minLength = (allRevsPlusBPs minBy (a => a._2.length))._2.length
        val candidates = (for (rbp <- allRevsPlusBPs; if (rbp._2.length == minLength) )
          yield (rbp._1, rbp._2))
        getNumReversals(revCount + 1, candidates.removeDuplicates)
      }
    }
    
    val pi = applyPermutation(sigma, inversePermutation(tau))
    getNumReversals(0, List((pi, breakpoints(pi))))
  }
}