package rosalind

object PCOV {
  def getCyclicSuperstr(kmers:List[String]):String = {
    def buildCycle(sofar:String, rest:List[String], overlap:Int):String = 
      rest.find(_.startsWith(sofar.substring(sofar.length-overlap))) match {
      case None => sofar.substring(0, sofar.length-overlap)
      case found => buildCycle(sofar+found.get.last, rest.remove(found.get.equals(_)), overlap)
    }
    
    buildCycle(kmers.head, kmers.tail, kmers.head.length-1)
  }
}