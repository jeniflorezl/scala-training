import org.scalatest.FunSuite
import org.scalatest.Matchers._

class RangesTest extends FunSuite{

  test("ranges"){
    val range = 0 until 10 by 10
    println(range)

    val someNumbers = Range(0,10)
    val second = someNumbers(1)
    val last = someNumbers.last

    someNumbers.size should be(10)
    second should be(1)
    last should be(9)

    //no include its upper bound
    val someNumbers2 = Range(2,10,3)
    val second2 = someNumbers2(1)
    val last2 = someNumbers2.last

    println(someNumbers2.size)
    println(second2)
    println(last2)

    //for including its upper bound
    val someNum = Range(0,10).inclusive
    someNum.last should be(10)
    
    //or
    val otherSomeNum = 0 to 10
    (someNum == otherSomeNum) should be(true)
  }

}
