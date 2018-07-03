import org.scalatest.FunSuite

class ImplicitsTest extends FunSuite{

  test("Implicits"){
    abstract class SemiGroup[A]{
      def add(x: A, y: A) : A
    }

    abstract class Monoid[A] extends SemiGroup[A] {
      def unit : A
    }

    object ImplicitTest extends App {
      implicit object StringMonoid extends Monoid[String]{
        def add(x: String, y: String): String = x concat(y)
        def unit: String = ""
      }

      implicit object IntMonoid extends Monoid[Int] {
        def add(x: Int, y: Int): Int = x + y
        def unit: Int = 0
      }

      def sum[A] (xs: List[A])(implicit m: Monoid[A]): A =
        if (xs.isEmpty) m.unit
        else m.add(xs.head, sum(xs.tail))
      println(sum(List(1,2,3)))
      println(sum(List("a", "b", "c")))
    }
  }

  test("implicit 2"){
    def howMuchCanIMake_?(hours: Int)(implicit  dollarPerHour: BigDecimal) =
      dollarPerHour * hours

    implicit val hourlyRate = BigDecimal(34)

    println(howMuchCanIMake_?(30))
  }

  test("implicit 3"){
    import java.math.BigInteger
    implicit def Int2BigIntegerConvert(value: Int): BigInteger =
      new BigInteger(value.toString)

    def add(a: BigInteger, b: BigInteger) = a.add(b)


    println(add(Int2BigIntegerConvert(3), Int2BigIntegerConvert(6)))
    println(Int2BigIntegerConvert(9))
    println(add(3,6).intValue())
    println((9: BigInteger))
  }

}
