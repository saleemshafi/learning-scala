package rosalind
import rosalind.KMER._
import rosalind.DBRU._

object GASM {
  
  def kmers(S:Set[String], k:Int):Set[String] = 
    for(s <- S; kmer <- KMER.kmers(s)(k)) yield kmer
  
  def getCycles(dbg:Set[(String,String)]):List[List[(String, String)]] = {
    def getCycles1(dbg:Set[(String, String)], cycles:List[List[(String,String)]], currentCycle:List[(String,String)]):List[List[(String,String)]] = 
      dbg.toSeq match {
	      case Seq() => currentCycle::cycles
	      case rest => rest.find( a => a._1 == currentCycle.last._2) match {
	        case None => getCycles1(dbg - dbg.head, currentCycle::cycles, List(dbg.head))
	        case Some(found) => getCycles1(dbg - found, cycles, currentCycle:+found)
	      }
      }
    println(dbg.head._1.length())
    getCycles1(dbg - dbg.head, Nil, List(dbg.head))
  }
  
  
  def cyclicString(cycle:List[(String, String)]) =
    cycle.reduceLeft((a,b) => (a._1+b._1.last, b._2)) match {
      case (full, last) => full.dropRight(last.length - 1)
    }

  
  def getSuperString(S:Set[String]):String = 
    (for (k <- (1 to S.head.length-1).reverse.toStream; 
    		dbg = getDeBrujinGraph(kmers(S,k+1));
    		cycles = getCycles(dbg);
    		if (cycles.length == 2)) yield cyclicString(cycles.head)).head
}