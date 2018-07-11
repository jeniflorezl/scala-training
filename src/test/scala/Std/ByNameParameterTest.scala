package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class ByNameParameterTest extends FunSuite{

  test("by-name parameter, there is no need to explicitly handle Unit or ()"){
    def cal(x: => Int): Either[Throwable, Int] = {
      //x is a call by name parameter
      try {
        Right(x)
      } catch {
        case b: Throwable => Left(b)
      }
    }

    val y = cal{
      println("Here we go!")
      val z = List(1,2,3,4)
      //must return a int
      49 + 20
    }

    y should be(Right(69))
  }

  test("parameters with object and apply"){
    object PigLatinizer {
      def apply(x: String) = x.tail + x.head + "ay"
    }

    val result = PigLatinizer {
      val x = "pret"
      val z = "zel"
      x ++ z //concatenate the strings
    }

    result should be("retzelpay")
  }

}
