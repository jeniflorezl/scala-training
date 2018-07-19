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

    //el map por defecto se ejecuta solo para el right
    tripleEither(Right(1)) should be(Right(3))
    tripleEither(Left("not a number")) shouldBe(Left("not a number"))

    //or

    def tripleEither2(either: Either[String, Int]): Either[String, Int] = either.left.map(triple2)
    def triple2(x: String) = "Hola " + x

    //el .left.map se ejecuta por izquierda
    tripleEither2(Right(1)) should be(Right(1))
    tripleEither2(Left("not a number")) shouldBe(Left("Hola not a number"))
  }

  def impar(x: Int): Either[String, Int] = {
    if (x % 2 != 0) Right(x) else Left(s"El numero $x es par")
  }

  test("Swap an Etiher Right"){
    val either1 = impar(3)
    either1 should be(Right(3))
    //intercambia
    val either2: Either[Int, String] = either1.swap
    either2 should be(Left(3))
  }

  test("Swap an Etiher Left"){
    val either1 = impar(2)
    either1 should be(Left("El numero 2 es par"))
    //intercambia
    val either2: Either[Int, String] = either1.swap
    either2 should be(Right("El numero 2 es par"))
  }

  test("for comp - todos Right"){
    val result = for {
      x <- impar(3)
      y <- impar(9)
      z <- impar(11)
    } yield x + y + z
    result should be(Right(23))
  }

  test("for comp - un Left - Se para la ejecuciòn del for comp"){
    val result = for {
      x <- impar(1)
      y <- impar(9)
      z <- impar(12)
    } yield x + y + z
    result should be(Left("El numero 12 es par"))
  }

  def cube = (x: Int ) =>  x * x * x
  def elevar = (x: Int, n: Int) => Math.pow(x, n)

  test("fold with eithers"){
    val either1 = Right(5)
    val either2 = Right(10)
    val result = either1.fold[Double](e => {
      cube(e)
    },
    {
      s2 => elevar(s2, 2)
    })
    result should be(25)

    val result2 = either2.fold[Double](e => {
      cube(e)
    },
      {
        s2 => elevar(s2, 2)
      })
    result2 should be(100)
  }

  test("prueba either"){
    val either: Either[Int, Int] = Right(3)
    val either2: Either[Int, Int] = Right(4)
    val either3: Either[Int, Int] = Right(4)
    val either4: Either[Int, Int] = Left(5)
    val res = for {
      e <- either
      e1 <- either2
      e2 <- either3
      e3 <- either4
    } yield e + e1 + e2 + e3

    assert(res == Left(5))
  }

  test("derecha si es par"){
    def par(x: Int):Either[String, Int] = {
      if (x %2==0) Right(x) else Left("Es impar")
    }

    val res = for {
      x <- par(2)
      y <- par(2)
      z <- par(2)
      t <- par(2)
    } yield x + y + z + t
    assert(res == Right(8))
  }



}
