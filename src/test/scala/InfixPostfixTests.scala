import org.scalatest.FunSuite
import org.scalatest.Matchers._

class InfixPostfixTests extends FunSuite{

  //Any method with take a single parameter
  test("infix operator"){
    val str = "hola"
    //this is
    (str + " world") should be("hola world")
    //the same
    str.+(" world") should be("hola world")
    val g: Int = 3
    println((g +(4)))
    println(g.+(4))
    (g +(4)) should be(7)
    g.+(4) should be(7)

    println(str indexOf('o', 0))

    val x: Int = 10
    println(x toHexString)

    val list = List(1,2,3,4,4,6,7,8)
    val list2 = list.filterNot(x => list.indexOf(x) % 2 == 0)
    println(list2)
  }

}
