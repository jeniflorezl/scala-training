import org.scalatest.FunSuite
import org.scalatest.Matchers._

class SetsTest extends FunSuite{

  //no dupplicate elements
  test("creating a set"){
    val mySet = Set("Jeniffer", "Angela", "Camila")
    mySet.size should be(3)
  }

  //subsets
  test("validate subsets"){
    val mySet1 = Set("Jeniffer", "Alejandra", "Camila")
    val mySet2 = Set("Jeniffer", "Alejandra")
    val mySet3 = Set("Jeniffer", "Carlos")

    mySet2 subsetOf mySet1 should be(true)
    mySet3 subsetOf mySet1 should be(false)
  }

  //diffrence
  test("difference") {
    val mySet=Set("Hola", "Adios", "Bye", "Hello")
    val mySet2 = Set("Hola", "Bye")
    val difference = mySet diff mySet2

    difference.equals(Set("Adios","Hello"))
  }
}
