package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class OptionsTest extends FunSuite{

  def maybeItWillReturnSomething(flag: Boolean) : Option[String] = {
    if (flag) Some("Something") else None
  }

  test("maybeItWillReturnSomethingTest"){
    val value1 = maybeItWillReturnSomething(true)
    val value2 = maybeItWillReturnSomething(false)
    value1 should be(Some("Something"))
    value2 should be(None)

    value1 getOrElse("No found value") should be("Something")
    value2 getOrElse("No found value") should be("No found value")

    value1.isEmpty should be(false)
    value2.isEmpty should be(true)
  }

  test("PatternMatching"){
    val someValue: Option[Double] = Some(20.0)
    val value = someValue match {
      case Some(v) => v
      case None => 0.0
    }

    value should be(20.0)

    val noneValue: Option[Double] = None
    val value2 = noneValue match  {
      case Some(v) => v
      case None => 0.0
    }

    value2 should be(0.0)
  }

  test("Option.Map"){
    val number: Option[Int] = Some(3)
    val noNumber:Option[Int] = None

    val result1 = number.map(_ *1.5)
    val result2 = noNumber.map(_*1.5)

    result1 should be(Some(4.5))
    result2 should be(None)
  }

  test("Option.Fold"){
    val number: Option[Int] = Some(3)
    val noNumber:Option[Int] = None

    val result1 = number.fold(1)(_*3)
    val result2 = noNumber.fold(1)(_*3)

    assertResult(9){
      result1
    }
    assert(result2===1)
  }

  test("manipulating options"){
    def cube(x: Double): Option[Double] = Some(x * x * x)

    def foo(x: Double): String = cube(x) match {
      case None => "no result"
      case Some(y) => y.toString
    }

    val result = foo(4.5)
    result should be("91.125")
  }

  test("filter"){
    val option : Option[String] = Some("Scala")
    option.filter(x => x.startsWith("f")) should be(None)
  }

  test("flatmap"){
    val option : Option[String] = Some("Scala")
    option.flatMap(x => Some(x + " is cool")) should be(Some("Scala is cool"))
  }
}
