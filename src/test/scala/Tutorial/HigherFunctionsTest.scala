package Tutorial

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class HigherFunctionsTest extends FunSuite{
  test("sum function uses linear recursion"){
    def sum(f: Int => Int, a: Int, b: Int) = {
      def loop(x: Int, acc: Int): Int = {
        if (x > b) acc
        else loop(x + 1, acc + f(x))
      }
      loop(a, 0)
    }

    sum(x => x, 1, 10 ) should be(55)
  }

}
