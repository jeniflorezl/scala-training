package Tutorial

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class SyntacticConveniencesTest extends FunSuite{
  test("string interpolation"){
    def greet(name: String) = s"Hello, $name"
    greet("Scala") should be("Hello, Scala")

    def greet2(name: String) = s"Hello, ${name.toUpperCase}"
    greet2("Scala") should be("Hello, SCALA")
  }

  test("tuples"){
    def pair(x: Int, s: String) : (Int, String) = (x, s)
    pair(42, "choes") should be((42, "choes"))

    val is : (Int, String) = (42, "beds")
    is match {
      case(i, s) =>
        i should be(42)
        s should be("beds")
    }

    val (i, s) = is
    i should be(42)
    s should be("beds")

    //otherWay
    is._1 should be(42)
    is._2 should be("beds")
  }

  test("expansion de functions values"){
    //this
    (x: Int) => x * x


    //to
    {
      class AnonFunc extends Function1[Int, Int]{
        def apply(x: Int) = x * x
      }
      val n = new AnonFunc
      n.apply(5) should be(25)
    }

    val f = (x: Int) => x * x
    f(2) should be(4)

  }

  test("for expressions - map"){
    val x = List(1,2,3)
    x.map(x => x * 2) should be(List(2,4,6))

    //with for expressions -- for every value, that I name ‘x’, in ‘xs’, return ‘x + 1’
    val result = for (xs <- x) yield xs * 2
    result should be(List(2,4,6))
  }

  test("for expressions - filter"){
    val x = List(1,2,3)
    x.filter(x => x % 2 == 0) should be(List(2))

    //with for expressions
    val result = for (xs <- x if xs % 2 == 0) yield xs
    result should be(List(2))
  }

  test("benefit of use for expression"){
    val x = List(1,2,3)
    val result = x.filter(x => x % 2 == 0).map(x => x + 1)
    result should be(List(3))

    val result2: List[Int] = for (y <- x if y % 2 == 0) yield y + 1
    result2 should be(List(3))
  }

  test("for expression - flatMap"){
    val x = List(1,2,3)
    val y = List(4,5,6)
    val result = x.flatMap(n => y.map(z => (n,z)))
    result should be(List((1,4), (1,5), (1,6), (2,4), (2,5), (2,6), (3,4), (3,5), (3,6)))

    //“for every value ‘x’ in ‘xs’, and then for every value ‘y’ in ‘ys’, return ‘(x, y)’”
    val result2 = for {
      xl <- x
      yl <- y
    } yield (xl, yl)
    result2 should be(List((1,4), (1,5), (1,6), (2,4), (2,5), (2,6), (3,4), (3,5), (3,6)))

    val result3 = for {
      xl <- x if xl % 2 == 0
      yl <- y
    } yield (xl,yl)

    result3 should be(List((2,4), (2,5), (2,6)))

    //the equivalente without for expression
    x.filter(x => x % 2 == 0).flatMap(xl => y.map(yl => (xl,yl))) should be(List((2,4), (2,5), (2,6)))
  }

  test("named parameters"){
    case class Range(start: Int, end: Int, step: Int = 1)
    val range = Range(1,10)
    range.step should be(1)
  }

  test("Repeated parameters"){
    def average(x: Int, xs: Int*) : Double =
      (x :: xs.to[List]).sum.toDouble / (xs.size + 1)

    average(1) should be(1.0)
    average(1, 2, 3) shouldBe(2)

    val list = List(1,2,3,4)
    average(1, list: _*) shouldBe(2.2)

  }

  test("Type aliases"){
    type Result = Either[String, (Int, Int)]
    def divide(dividen: Int, divisor: Int) : Result =
      if (divisor == 0) Left("division por zero")
      else Right((dividen / divisor),(dividen % divisor))

    divide(4,2) should be(Right(2,0))
    divide(3,0) shouldBe(Left("division por zero"))
  }


}
