package rosalind

import org.scalatest.FunSuite
import rosalind.REAR._
import scala.collection.mutable.Stack
 
class SORTTestSuite extends FunSuite {
  
  def IntList(data:String) = data.split(" ").map(Integer.parseInt(_)).toList

  test("sorting by reversals with data from problem") {
    val start = IntList("1 2 3 4 5 6 7 8 9 10")
    val end = IntList("1 8 9 3 2 7 6 5 4 10")
    val reversals = getReversals(start, end)
    expect(2) { reversals.length }
    expect((4,9)) { reversals.head }
    expect((2,5)) { reversals.last }
  }
  
  test("sorting by reversals with real data") {
    val data = """2 5 9 6 4 7 1 8 10 3
3 6 2 9 7 5 8 4 10 1""".split("\r\n").toList
    val reversals = getReversals(IntList(data.head), IntList(data.last))
    println(reversals.length)
    reversals foreach {
      r => println (r._1 + " " + r._2)
    }
  }
}