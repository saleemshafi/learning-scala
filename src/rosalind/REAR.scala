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

  def getReversals(sigma:Permutation, tau:Permutation):List[(Int,Int)] = {
    
    @tailrec
    def getIdentityReversals(perms:List[(Permutation, List[(Int, Int)], List[Breakpoint])]):List[(Int, Int)] = perms match {
      case Nil => Nil
      case (perm, reversals, Nil)::anything => reversals
      case _ => {
      	val allRevsPlusBPs = (for ((pi, prevRevs, bps) <- perms; (start, end) <- breakpointCombos(bps); r = reverse(pi, start-1, end-1)) 
      	  yield (r, prevRevs:+(start, end), breakpoints(r)))
      	val minLength = (allRevsPlusBPs minBy (a => a._3.length))._3.length
        val candidates = (for (rbp <- allRevsPlusBPs; if (rbp._3.length == minLength) )
          yield (rbp._1, rbp._2, rbp._3))
        getIdentityReversals(candidates.removeDuplicates)
      }
    }
    
    val pi = applyPermutation(sigma, inversePermutation(tau))
    getIdentityReversals(List((pi, Nil, breakpoints(pi))))
  }
  def getNumReversals(sigma:Permutation, tau:Permutation):Int = {
    getReversals(sigma, tau).length
  }
}