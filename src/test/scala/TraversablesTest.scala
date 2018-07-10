import org.scalatest.FunSuite
import org.scalatest.Matchers._

class TraversablesTest extends FunSuite{
  test("traversables"){
    val list = List(1,2,3)
    val set = Set(4,5,6)
    val result = list ++ set
    val result2 = set ++ list
    result should be(List(1,2,3,4,5,6))
    result2 should be(Set(4,5,6,1,2,3))
    println(result.size)
    println(result2.size)

    val result3 = set.map(_*4)
    result3.lastOption should be(Some(24))
  }

  test("pack all into a single traversable"){

  }
}
