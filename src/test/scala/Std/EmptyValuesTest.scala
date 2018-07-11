package Std

import org.scalatest.FunSuite

class EmptyValuesTest extends FunSuite{
  test("none can be converted to String"){
    println(None.toString)
  }

  test("Some is the opposite of None for Option types"){
    val optional: Option[String] = Some("Some Value")
    assert((optional == None) === false, "Some(value) should not equal None")
    assert(optional.isEmpty === false, "Some(value) should not be empty")
  }

  test("literal numbers"){
    val num = 0x30F
    println(num)
  }


}
