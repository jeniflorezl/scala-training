package Std


import org.scalatest.FunSuite
import org.scalatest.Matchers._

class PointTest extends FunSuite{
  test("Point.toString"){
    val point = new Point(1,2)
    //assert((1,2)==point)
    //point shouldEqual (1,2)
    /*assertResult((1,2)){
      point
    }
    point.toString() should be("(1,2)")*/
  }


  test("ClassWithValParameter") {
    val instancia = new ClassWithValParameter("Adrian")
    instancia.name should be("Adrian")
  }
}
