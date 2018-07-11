package Tutorial

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class TermsAndTypesTest extends FunSuite{

  test("type and terms"){
    (1+2) should be(3)
    "Hello, " ++ "Scala" should be("Hello, Scala")
    "Hello".size should be(5)

    //.abs return the absolute value of a number
    -42.abs should be(42)

    //infix type allow you to omit the dot and the parentheses
    assert(3 + 2 === 3.+(2))

    println(16.toHexString)

  }
}
