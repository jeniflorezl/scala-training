package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class OptionsTest extends FunSuite{

  test("creating an option"){
    //with value
    val opt = Option(1)
    opt should be(Some(1))

    //without value
    val opt1 = None
    opt1 should be(None)
  }

  test("acceder al valor de un option de forma segura"){
    //with value
    val opt = Option(List(1,2,3))
    opt getOrElse("empty") should be(List(1,2,3))

    //without value
    val opt1 = None
    opt1 getOrElse("empty") should be("empty")


  }

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

  test("know if an option has value"){
    val opt = Option(2)
    opt.isDefined should be(true)
  }

  test("Option.Map"){
    val number: Option[Int] = Some(3)
    val noNumber:Option[Int] = None

    val result1 = number.map(_ *1.5)
    val result2 = noNumber.map(_*1.5)

    result1 should be(Some(4.5))
    result2 should be(None)
  }

  test("transformar una lista de options"){
    val list = List(Some("Pedro"), Some("Ricardo"), Some("Luis"), None)
    val result = list.map(x => Option(x getOrElse("none") toUpperCase))
    result should be(List(Some("PEDRO"), Some("RICARDO"), Some("LUIS"), Some("NONE")))
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

  test("Option.Fold 2"){
    val op: Option[Int] = None

    val res: Int = op.fold{
      10
    }{
      x => x + 4
    }
    assertResult(10){
      res
    }
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

  test("for comprehensions en Options"){
    val opt = Option(2)
    val opt2 = Option("hola")
    val opt3 = None

    //llega hasta donde halla valor
    val result = for {
      o1 <- opt
      o2 <- opt2
      o3 <- opt3
    } yield o1 + o2 + o3
    result should be(None)

    //devuelve en el mismo efecto
    val result2 = for {
      o1 <- opt
      o2 <- opt2
    } yield o1 + o2
    result2 should be(Some("2hola"))
  }


  //evitando usar pattern matching
  def foo(x: String) = x.toUpperCase
  val option: Option[String] = Some("Scala")

  test("With flatMap"){
    val result = option match {
      case None => None
      case Some(x) => foo(x)
    }
    result should be("SCALA")

    //mas bien
    val result2 = option.flatMap(x => Some(foo(x)))

    result2 should be(Some("SCALA"))
  }

  test("With flatten"){
    val option = Option(Option("Scala"))
    val res = option match {
      case None => None
      case Some(x) => x
    }

    res should be(Some("Scala"))

    val res1 = option.flatten

    res1 should be(Some("Scala"))
  }

  test("With map"){
    val res = option match {
      case None => None
      case Some(x) => Some(foo(x))
    }
    res should be(Some("SCALA"))

    val res1 = option.map(x => foo(x))
    res1 should be(Some("SCALA"))
  }

  /*test("With foreach"){
    def foo(x: String) = println(x.toUpperCase)
    val res = option match {
      case None => {}
      case Some(x) => foo(x)
    }

    res should be("SCALA")

    val res1 = option.foreach(x => foo(x))
    println(res1)
    res1 should be("SCALA")
  }*/

  test("isDefined"){
    val res = option match {
      case None => false
      case Some(x) => true
    }
    res should be(true)

    val res1 = option.isDefined
    res1 should be(true)

  }

}
