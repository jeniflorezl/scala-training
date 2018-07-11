package Std

import java.applet.Applet

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class LiteralNumbersTest extends FunSuite{
  test("Integer literals"){
    val a = 2
    val b = 31
    val c = 0x30F
    val e = 0
    val f = -2
    val g = -31
    val h = -0x30F
    a should be(2)
    b should be(31)
    c should be(783) //Hint: 30F = 783
    e should be(0)
    f should be(-2)
    g should be(-31)
    h should be(-783)
  }

  test("Long literals"){
    val a = 2L
    val b = 31L
    val c = 0x30FL
    val e = 0L
    val f = -2l
    val g = -31L
    val h = -0x30FL
    a should be(2)
    b should be(31)
    c should be(783) //Hint: 30F = 783
    e should be(0)
    f should be(-2)
    g should be(-31)
    h should be(-783)
  }

  test("float and double literals"){
    val a = 3.0
    val b = 3.00
    val c = 2.73
    val d = 3f
    val e = 3.22d
    val f = 93e-9
    val g = 93E-9
    val h = 0.0
    val i = 9.23E-9D
    a should be(3.0)
    b should be(3.00)
    c should be(2.73) //Hint: 30F = 783
    e should be(3.22)
    f should be(93e-9)
    g should be(93e-9)
    h should be(0)
    i should be(9.23E-9D)

    //TypeVariance

    class MyContainer[+A](val a: A)(implicit manifest: scala.reflect.Manifest[A]) {
      def contents = manifest.runtimeClass.getSimpleName
    }

    class Fruit
    class Orange extends Fruit
    class Apple extends Fruit
    class Tangelo extends Orange

    val fruitBasket: MyContainer[Fruit] = new MyContainer[Orange](new Tangelo())
   // assertResult("Fruit")(fruitBasket.contents)

    val fruitBasket2: MyContainer[Fruit] = new MyContainer[Apple](new Apple)
    }



}
