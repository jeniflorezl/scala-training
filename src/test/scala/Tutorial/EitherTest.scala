package Tutorial

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class EitherTest extends FunSuite{
  test("using eithers"){
    val either: Either[String, Int] = Right(1)

    //Map
    val result: Either[String, Int] = either.map(x => x + 1)
    result should be(Right(2))

    //FlatMap
    val result2: Either[String, Int] = either.flatMap(x => Right(x + 1))
    result2 should be(Right(2))
  }

  def MyMap[A,B](either: Either[A, B], actionLeft: (A) => A, actionRight: (B) => B) = {
    if (either.isRight) either.map(actionRight) else either.left.map(actionLeft)
  }

  test("MyMap"){
    val either: Either[String, Int] = Right(2)
    val either2: Either[String, Int] = Left("Hello")
    MyMap[String, Int](either,(y: String) => "Scala", (x: Int) => x+2) should be(Right(4))
    MyMap[String, Int](either2,(y: String) => y + " Scala", (x: Int) => x+2) should be(Left("Hello Scala"))

  }

  test("test 2"){
    def triple(x: Int) = 3 * x
    def tripleEither(either: Either[String, Int]) = either.map(triple)

    tripleEither(Right(1)) should be(Right(3))
    tripleEither(Left("not a number")) shouldBe(Left("not a number"))
  }

}
