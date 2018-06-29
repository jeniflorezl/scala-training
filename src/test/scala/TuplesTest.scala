import org.scalatest.FunSuite
import org.scalatest.Matchers._

class TuplesTest extends FunSuite{

  test("Testin tuples"){
    val tuple = ("apple", "dog")
    val fruit = tuple._1
    val animal = tuple._2

    fruit should be("apple")
    animal should be("dog")
  }

  test("Testin assgin multiple variables"){
    val student = ("Adrian", 21, 3.4)
    val (name, age, gpa) = student

    name should be("Adrian")
    age should be(21)
    gpa should be(3.4)
  }
}
