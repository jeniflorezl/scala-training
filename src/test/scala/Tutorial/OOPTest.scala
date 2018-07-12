package Tutorial

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class OOPTest extends FunSuite{
  test("example rational arithmetic"){
    class Rational(x: Int, y: Int){
      def numer = x
      def dem = y
      def add(r1: Rational) = {
        new Rational((numer * r1.dem + r1.numer * dem), (dem * r1.dem))
      }
      def rest(r1: Rational) = {
        new Rational((numer * r1.dem - r1.numer * dem), (dem * r1.dem))
      }
      def mul(r1: Rational) = {
        new Rational((numer * r1.numer), (dem * r1.dem ))
      }
      def div(r1: Rational) = {
        new Rational((numer * r1.dem ), (dem * r1.numer))
      }
      override def toString: String = numer + "/" + dem
    }
    val rational1 = new Rational(1,2)
    val rational2 = new Rational(3,4)
    rational1.add(rational2).toString should be("10/8")
    rational1.rest(rational2).toString should be("-2/8")
    rational1.mul(rational2).toString should be("3/8")
    rational1.div(rational2).toString should be("4/6")
  }

  test("simplifying"){
    //numer and dem are call infrequently
    class Rational(x: Int, y: Int){
      private def gcd(a: Int, b: Int): Int  = if(b == 0) a else gcd(b, a % b)
      private val g = gcd(x,y)
      def numer = x / g
      def dem = y / g

      def less(that: Rational) =
        numer * that.dem < dem * that.numer

      def max(that: Rational) = {
        if(less(that)) that else this
      }

      require(y>0, "denominator must be positive")

      //auxiliary constructor
      def this(x: Int) = this(x, 1)

    }

    //numer and dem are call often
    class Rational2(x: Int, y: Int){
      private def gcd(a: Int, b: Int): Int  = if(b == 0) a else gcd(b, a % b)
      val numer = x / gcd(x,y)
      val dem = y / gcd(x,y)

    }

    val rational = new Rational(1,2)
    println(rational.numer)
    println(rational.dem)
  }

}
