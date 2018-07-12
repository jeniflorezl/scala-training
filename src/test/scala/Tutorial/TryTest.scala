package Tutorial

import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.util.{Failure, Success, Try}

class TryTest extends FunSuite{

  def cube(x: Double) : Try[Double] = {
    if (x < 0) Failure(new IllegalArgumentException("x must be positive"))
    else Success(x * x * x)
  }

  test("testing try"){
    val result = cube(5)
    result should be(Success(125))
  }

  test("pattern matching"){
    def validate(x: Double): String = cube(x) match {
      case Failure(_) => "no result"
      case Success(x) => x.toString
    }

    //or
    def validate2(x: Double): String = cube(x) match {
      case Failure(e) => e.getMessage
      case Success(x) => x.toString
    }

    val result = validate(8)
    result should be("512.0")

    val result2 = validate(-1)
    result2 should be("no result")

    val result3 = validate2(-1)
    result3 should be("x must be positive")
  }

  test("filter"){
    val list: Try[List[Int]] = Success(List(1,2,3,4))
    val result: Try[List[Int]] = list.map(l => l.filter(x => x % 2 == 0))
    result should be(Success(List(2,4)))
  }

  test("flatMap"){
    def sum(a: Int, b: Int): Try[Int] = {
      if (a < 0 && b < 0) Failure(new IllegalArgumentException("must be positives"))
      else Success(a + b)
    }

    def rest(a: Int, b: Int): Try[Int] = {
      if (a < 0 || b < 0) Failure(new IllegalArgumentException("must be positives"))
      else if (a < b) Success(b-a)
      else Success(a-b)
    }

    def mul(a: Int, b: Int): Try[Int] = {
      if (a < 0 && b < 0) Failure(new IllegalArgumentException("must be positives"))
      else Success(a*b)
    }

    val result : Try[Int] = sum(4,8).flatMap(x => rest(x, 5)
                                      .flatMap(y => mul(y, 9)))
    result should be(Success(63))

    val result2 : Try[Int] = sum(4,8).flatMap(x => rest(x, -2).recoverWith{ case e => Try{1}}
      .flatMap(y => mul(y, 9)))
    result2 should be(Success(9))

    val list: Try[List[String]] = Success(List("Mario", "Camilo", "Andrea", "Sofia"))

    val result3: Try[List[String]] = list.flatMap(l => Try{l.map(x => x.toUpperCase)})
                                            .flatMap(l2 => Try{l2.filter(l2 => l2.startsWith("M"))})

    result3 should be(Success(List("MARIO")))

    val result4 = list.map(l => l.map(x => x.toUpperCase))
      .map(l2 => l2.filter(l2 => l2.startsWith("M")))

    result4 should be(Success(List("MARIO")))

  }

}
