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

    //it does not infix operator
    println(str indexOf('o', 0))

  }

  //does not have parameters, + - ! ~
  test("postfix operator"){

    //postfix
    val x: Int = 31
    println(x toHexString)
    println((-x))

    class MyPerson{
      def unary_+ = "Hola"
      def unary_- = "Chao"
    }

    val person = new MyPerson
    +person should be("Hola")
    -person should be("Chao")
  }

}
