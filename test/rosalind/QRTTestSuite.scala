package rosalind

import org.scalatest.FunSuite
import rosalind.CONV._
import scala.collection.mutable.Stack
 
class QRTTestSuite extends FunSuite {
  def getTaxa(character:String, names:List[String]):(List[String], List[String]) = {
    (character zipWithIndex).foldLeft((List[String](), List[String]())) { 
      case (l, ('x', i)) => l
      case (l, ('1', i)) => (names(i) +: l._1, l._2)
      case (l, ('0', i)) => (l._1, names(i) +: l._2)
    }
  }
  
  def getQuartets(names:List[String], characters:List[String]):Set[Set[Set[String]]] = {
    (for (c <- characters; 
    		(in, out) = getTaxa(c, names);
    		pi <- in.combinations(2);
    		po <- out.combinations(2))
      yield Set(Set(pi(0), pi(1)), Set(po(0), po(1)))).toSet
  }
  
  test("quartets with data from problem") {
    val data = """cat dog elephant ostrich mouse rabbit robot
01xxx00
x11xx00
111x00x""".split("\r\n").toList
    val names = data.head.split(" ").toList
    val characters = data.tail
    
    val quartets = getQuartets(names, characters) 
    quartets foreach {
      q => {
    	  val ql = q.toList
    	  println("{%s} {%s}".format(ql.head.mkString(", "), ql.last.mkString(", ")))  
      }
    }
    
    expect(4) { quartets.size }
    assert( quartets.contains(Set(Set("dog", "elephant"),Set("rabbit","robot"))) )
    assert( quartets.contains(Set(Set("cat", "dog"),Set("mouse","rabbit"))) )
    assert( quartets.contains(Set(Set("cat","elephant"),Set("mouse", "rabbit"))) )
    assert( quartets.contains(Set(Set("dog", "elephant"),Set("mouse","rabbit"))) )
  }

  test("quartets with real data") {
    val data = """Ahaetulla_maculatum Apalone_gigas Babycurus_trianguligerus Buteo_wumuzusume Cardiocranius_vermiculatus Chelodina_mandarina Citharacanthus_purpurascens Ctenotus_flavescens Cuora_marinus Dyscophus_mexicanum Hadrurus_capreolus Megophrys_aureola Paradoxornis_marinus Platemys_ladogensis Salmo_laticauda Squaterola_dexter Tursiops_virgo Upupa_vulpes
100000x0x0000x10x0
000000000001000100
1xxxx1xx1xx1111x00
x11xx00xx0x110x100
xxx0xxx1xxxxxxxxxx
x1100xxx1xxxxxxxx0
100001x00000001000
xx10xxxxxxxx1xxx0x
1xx01x1xxxxxxxxxxx
xx0x1x1xx10xxxxxxx
1xxxxxxxxxxxxxx10x
010000x0xxx0100000
001xx00000xx0xxx00
x01x1x1x1xx10xxx1x
111xx1x101111011x1""".split("\r\n").toList
    val names = data.head.split(" ").toList
    val characters = data.tail
    
    getQuartets(names, characters) foreach {
      q => {
    	  val ql = q.toList
    	  println("{%s} {%s}".format(ql.head.mkString(", "), ql.last.mkString(", ")))  
      }
    }
  }
}