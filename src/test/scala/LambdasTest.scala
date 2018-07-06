import java.util.function.BiFunction

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class LambdasTest extends FunSuite {

  test("Testing lambdas") {
    def lambda = {
      x: Int => x + 2
    }

    //Aplica la lambda
    val result1 = lambda.apply(5)

    assert(result1 == 7)

    //Recibe un Int y devuelve un Int
    val lambda2 = new Function[Int, Int] {
      def apply(v1: Int): Int = v1 - 1
    }

    val result2 = lambda2.apply(7)

    assertResult(6) {
      result2
    }
  }

  test("Testing closure") {
    var incrementer = 1

    def closure = { x: Int => x + incrementer }

    val result1 = closure(10)

    assertResult(11) {
      result1
    }

    incrementer = 2

    val result2 = closure(10)

    assertResult(12) {
      result2
    }
  }

  test("Test like higher order function") {
    def summation(x: Int, y: Int => Int) = y(x)

    var incrementer = 3

    def closure = (x: Int) => x + incrementer

    //enviar lambda como parametro
    val result = summation(10, closure)

    assert(result == 13)

    incrementer = 5

    val result1 = summation(10, closure)

    assert(result1 == 15)

  }

  test("Higher order function returning another function") {
    def addWithoutSyntaxSugar(x: Int): Function1[Int, Int] = {
      new Function1[Int, Int]() {
        def apply(y: Int): Int = x + y
      }
    }

    def addWithoutSyntaxSugar2(x: Int) = (y: Int) => x + y

    addWithoutSyntaxSugar2(1).isInstanceOf[Function1[Int, Int]] should be(true)
    addWithoutSyntaxSugar2(2)(3) should be(5)

    def fiveAdder2: Function1[Int, Int] = addWithoutSyntaxSugar2(5)
    fiveAdder2(5) should be(10)
  }

  test("Test lambda map"){
    def makeUpper(x: List[String]) = x map {
      _.toUpperCase
    }

    def makeWhatEverYouLike(s: List[String], sideEffect : String => String) = s map sideEffect

    makeUpper(List("abd", "xyz", "123")) should be(List("ABD", "XYZ", "123"))

    makeWhatEverYouLike(List("ABC", "XYZ", "123"), {x => x.toLowerCase()}) should be(List("abc", "xyz", "123"))

    //usign it inline
    val myName = (name: String) => s"My name is $name"
    makeWhatEverYouLike(List("John", "Mark"), myName) should be(List("My name is John", "My name is Mark"))

    List("Scala", "Erlang", "Clojure") map (_.length) should be(List(5,6,7))
  }

  test("calculator"){
    def calculo(a: Int, b: Int, operacion: (Int, Int) => Int) : Int ={
      operacion(a,b)
    }
    def sum(a: Int, b: Int):Int = calculo(a, b, (a,b) => a + b)

    sum(3,4) should be(7)
  }

  test("returning a function"){
    def mul(a: Int, b: Int) = {
      (x: Int, y: Int) => a*b + x*y
    }

    def result = mul(2,8)
    val result2 = result(3,7) should be(37)
  }

  test("nest methods"){
    def factorial(x: Int)={
      def fact(x: Int, acc: Int): Int={
        if (x <=1) acc
        else fact(x-1, x * acc)
      }
      fact(x, 1)
    }

    factorial(2) should be(2)
  }

}
