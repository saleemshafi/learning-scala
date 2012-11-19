package rosalind

import scala.math.BigDecimal.RoundingMode

object TREE extends App with RosalindProblem {
  type Nodes = Set[Int]
  def mergeTrees(trees:List[Nodes], edges:List[(Int,Int)]):List[Nodes] = edges match {
    case Nil => trees
    case h::t => {
      val firstSetNum = trees.indexWhere( s => s.contains(h._1))
      val secondSetNum = trees.indexWhere( s => s.contains(h._2))
      mergeTrees(trees.updated(firstSetNum, trees(firstSetNum).union(trees(secondSetNum))).patch(secondSetNum, Nil, 1), t)
    }
  }
  
  val (maxData, edgesData) = getInput("TREE.input").splitAt(1)
  val max = maxData.head.toInt
  val edges = edgesData.map(e => {
    val nums = e.split(" ")
    (nums.head.toInt, nums.tail.head.toInt)
  }).toList
  val subtrees = (1 to max).map(n => Set(n)).toList
  val mergedTrees = mergeTrees(subtrees, edges) 
  println( mergedTrees.length - 1)
}