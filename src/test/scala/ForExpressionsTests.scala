import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.mutable.ListBuffer

class ForExpressionsTests extends FunSuite{

  test("for expressions can nest"){
    val xValues = 1 to 5
    val yValues = 1 to 3
    val coordinates = for {
      x <- xValues
      y <- yValues
    } yield (x, y)
    println(coordinates)
    println(coordinates(4))
  }

  test("Using for we can make more readable code"){
    val nums = List(List(2,3), List(1,4), List(3,2,5), List(11,23,4))
    val result = for {
      lista <- nums
      num <- lista
      if (num % 2 == 0)
    } yield (num)

    result should be(List(2,4,2,4))
  }

  test("Using for we can make more readable code 2"){
    val str = List(List("hola", "bye", "chao"), List("adios", "hello", "how are you"), List("fine", "good morning"))
    val result2 = for {
      lista <- str
      strL <- lista
      if (strL.startsWith("h"))
    } yield (strL)

    result2 should be(List("hola", "hello", "how are you"))
  }

  test("removing odd positions2"){
    val list = List(1,2,3,4,5,6,7)

  }

  test("flatMap with another function"){
    def repeat(n: Int, x: Int) = {
      val list = ListBuffer.empty[Int]
      for (_ <- 1 to n){
        list += x
      }
      list.toList
    }

    val list = List(1,2,3,4)
    val n = 5
    val result = list.flatMap(x => repeat(n, x))
    println(result)
  }

  test("removing odd positions"){
    val lis = List(1,2,3,4,4)
    val result = (lis zipWithIndex).collect{ case (n, index) if index % 2 != 0 => n }
    result should be(List(2,4))
  }





}
