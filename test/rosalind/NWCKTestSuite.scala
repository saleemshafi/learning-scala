package rosalind

import org.scalatest.FunSuite

import rosalind.NWCK._


class NWCKTestSuite extends FunSuite with RosalindProblem {
  test("tree node distance with data from problem") {
	  expect(1) { Tree.parseNewick("(cat)dog;").distance("dog", "cat") }
	  expect(2) { Tree.parseNewick("(dog,cat);").distance("dog", "cat") }
  }
  
  test("tree node weighted distance with data from problem") {
	  expect(75) { Tree.parseNewick("(dog:42,cat:33);").distance("cat", "dog") }
	  expect(136) { Tree.parseNewick("((dog:4,cat:3):74,robot:98,elephant:58);").distance("dog", "elephant") }
  }
  
  test("tree node distance with real data") {
    val data = getInput("NWCK_1.input")
    
	data.grouped(3).foreach {
      case data::nodes::rest => {
    	  val n = nodes.split(" ")
    	  print(Tree.parseNewick(data).distance(n.head, n.tail.head)+" ")
      }
      case _ => throw new Error
    }
  }

  test("tree node weighted distance with real data") {
    val data = getInput("NWCK_2.input")
    
	data.grouped(3).foreach {
      case data::nodes::rest => {
    	  val n = nodes.split(" ")
    	  print(Tree.parseNewick(data).distance(n.head, n.tail.head)+" ")
      }
      case _ => throw new Error
    }
  }
}