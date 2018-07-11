package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class TuplesTest extends FunSuite{

  test("Testin tuples"){
    val tuple = ("apple", "dog")
    val fruit = tuple._1
    val animal = tuple._2

    fruit should be("apple")
    animal should be("dog")
  }

  test("Testing assgin multiple variables"){
    val student = ("Adrian", 21, 3.4)
    val (name, age, gpa) = student

    name should be("Adrian")
    age should be(21)
    gpa should be(3.4)
  }



  test("test for"){
    var n = 10
    for(_ <- 1 to n){
      println("hola "+ n)
    }
    def hola(x: Int)(y: Int) = x + y

    hola(5)(2) should be(7)

  }

  test("test flatmap"){
    def function(n: Int, list: List[Int]) = list.flatMap(List.fill(n)(_))

    function(4, List(1,2,3)) should be(List(1,1,1,1,2,2,2,2,3,3,3,3))
  }
}
