import org.scalatest.FunSuite
import org.scalatest.Matchers._

class PartiallyAppliedFunctionsTest extends FunSuite{

  def sum(a: Int, b: Int, c: Int) = a + b + c

  test("partially applied functions") {
    val sum3= sum _
    sum3(1,9,7) should be(17)
    sum(4,5,6) should be(15)

  }

  test("replacing any number of arguments"){
   val sumC = sum(1, 10, _: Int)
    sumC(4) should be(15)
    sum(4,5,6) should be(15)
  }

  test("currying a function"){
    def multiply(x: Int, y: Int) = x * y
    val multiplyCurried = (multiply _).curried
    multiply(4,5) should be(20)
    multiplyCurried(3)(2) should be(6)
    val multiplyCurriedFour = multiplyCurried(4)
    multiplyCurriedFour(2) should be(8)
    multiplyCurriedFour(4) should be(16)
  }

  test("specialized versions of generalized functions"){
    def customFilter(f: String => Boolean)(xs : List[String]) = xs filter f
    def onlyEven(x: String) = x.startsWith("a")
    val xs = List("angela", "camila", "andrea", "sara", "dora")
    customFilter(onlyEven)(xs) should be(List("angela", "andrea"))

    val onlyEvenFilter = customFilter(onlyEven)_
    onlyEvenFilter(xs) should be(List("angela", "andrea"))
  }



}
