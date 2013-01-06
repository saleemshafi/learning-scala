package rosalind

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
	        case ':' => "[():;,]".r.findFirstMatchIn(newick.tail) match {
	            case None => null
	            case Some(m) => {
	              current.weight = Integer.parseInt(newick.tail.substring(0, m.start))
	              parse(newick.tail.substring(m.start), current)
	            }
	          }
	        case c => "[():;,]".r.findFirstMatchIn(newick.tail) match {
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
	  def weightSums(nodes:List[Node]):Int =
	    nodes.foldLeft(0)((w,n) => n.weight + w)
    
	  def distance(a: String, b: String): Int = (this.root.find(a), this.root.find(b)) match {
	    case (Some(aNode), Some(bNode)) => {
	      val aParents = aNode.parents
	      val bParents = bNode.parents
	      val incommon = (aParents intersect bParents)
	      weightSums(aParents filterNot (incommon contains)) + weightSums(bParents filterNot (incommon contains))
	    }
	    case _ => -1
	  }
	  
	  def characterTable:List[String] = {
	    var characters = this.root.characters
	    (for (ch <- characters) yield this.taxa.map( t => if (ch.contains(t)) '1' else '0' ).mkString).toList
	  }
	  
	  def taxa = this.root.allLeaves.sorted(Ordering.String)
  }
  
  class Node(val parent:Node) {
    var name = "node-"+counter
    var weight = 1
    var children = List[Node]()
    
    def this() = this(null)
    def addChild = {
      val child = new Node(this)
      this.children = this.children :+ child
      child
    }
    def addSibling = this.parent.addChild
    def parents:List[Node] = if (this.parent == null) List(this)
    							else this :: this.parent.parents
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
    
    def allLeaves:List[String] = this.children match {
      case Nil => List(this.name)
      case _ => for (child <- this.children; leaf <- child.allLeaves) yield leaf
    }
    
    def isLeaf = children.length == 0
    
    def characters: List[Set[String]] = {
      val nonLeaves = for (n <- this.children; if (!n.isLeaf)) yield n
      val theseCharacters = for (n <- nonLeaves) yield n.allLeaves.toSet
      val subCharacters = for (n <- nonLeaves; c <- n.characters) yield c
      
      (theseCharacters ++ subCharacters).toList
    }
  }
}