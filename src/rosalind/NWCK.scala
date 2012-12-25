package rosalind

import rosalind.SPEC._

object NWCK extends App with RosalindProblem {
  var _counter = 0
  def counter:Int = {
    _counter += 1
    _counter
  }
  
  object Tree {
    def parseNewick(newick:String):Tree = {
      def parse(newick:String, current:Node):Node = 
        newick.head match {
	        case ';' => current
	        case ')' => parse(newick.tail, current.parent)
	        case ',' => parse(newick.tail, current.addSibling)
	        case '(' => parse(newick.tail, current.addChild)
	        case c => "[();,]".r.findFirstMatchIn(newick.tail) match {
	            case None => null
	            case Some(m) => {
	              current.name = c+newick.tail.substring(0, m.start)
	              parse(newick.tail.substring(m.start), current)
	            }
	          }
	      }
      
      new Tree(parse(newick, new Node()))
    }
  }
  
  class Tree(val root: Node) {
	  def distance(a: String, b: String): Int = (this.root.find(a), this.root.find(b)) match {
	    case (Some(aNode), Some(bNode)) => {
	      val aParents = aNode.parents
	      val bParents = bNode.parents
	      val incommon = (aParents intersect bParents)
	      (aParents filterNot (incommon contains)).length + (bParents filterNot (incommon contains)).length
	    }
	    case _ => -1
	  }
  }
  
  class Node(val parent:Node) {
    var name = "node-"+counter
    var children = List[Node]()
    
    def this() = this(null)
    def addChild = {
      val child = new Node(this)
      this.children = this.children :+ child
      child
    }
    def addSibling = this.parent.addChild
    def parents:List[String] = if (this.parent == null) List(this.name)
    							else this.name :: this.parent.parents
	def find(name:String): Option[Node] = 
    	if (name.equals(this.name)) {
    		Some(this)
    	} else {
    		val finds = for (child <- this.children; find <- child.find(name)) yield find
    		finds match {
    		  case Nil => None
    		  case find::_ => Some(find)
    		}
    	}
  }
}