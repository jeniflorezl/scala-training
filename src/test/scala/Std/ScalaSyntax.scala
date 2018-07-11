package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class ScalaSyntax extends FunSuite {

  test("declaring variables mutables and inmutables") {

    //mutables
    var num = 1
    num = 2
    num should be(2)

    //specifing type
    var x: Int = 4

    x should be(4)

    //inmutables
    val num2 = 3
    assertDoesNotCompile("num2 = 4")
  }

  test("declaring a function"){
    val list = List(1,2,3,4)
    val funcion = (x: Int) => x * 2
    def p(list: List[Int], funcion: Int => Int) = {
      val list2 = list.map(l => funcion(l))
      list2
    }

    println(p(list,funcion))

  }


}
