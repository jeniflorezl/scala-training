package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class NamedDefaultArgumentsTest extends FunSuite{

  test("use name of variables explicitly"){
    def printName(first: String = "Jhon", last: String = "Suarez"): String ={
      val name = first + ", " + last
      name
    }

    printName("Angel", "Gonzales") should be("Angel, Gonzales")
    printName(first = "Esteban", "Cardona") should be("Esteban, Cardona")
    printName(last = "Lopez", first = "Daniel") should be("Daniel, Lopez")

    printName(last = "Londoño") should be("Jhon, Londoño")
  }

  test("default parameters can be functional too"){
    def reduce(a: Int, f: (Int, Int) => Int = _ + _): Int = f(a, a)
    reduce(5) should be(10)
    reduce(5, _ * _) should be(25)
  }

}
