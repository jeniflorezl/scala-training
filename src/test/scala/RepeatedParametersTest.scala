import org.scalatest.FunSuite
import org.scalatest.Matchers._

class RepeatedParametersTest extends FunSuite{

  test("a repeated parameter must be the last parameter"){
    def repeatedParameterMethod(x: Int, y: String, z: Any*) = {
      ("%d %ss can give you %s").format(x, y, z.mkString(", "))
    }

    repeatedParameterMethod(3, "egg", "a delicious sandwich", "protein", "high cholesterol") should be(
      "3 eggs can give you a delicious sandwich, protein, high cholesterol")
  }

  test("a repeated parameter can accept a collection"){
    def repeatedParameterMethod(x: Int, y: String, z: Any*) = {
      ("%d %ss can give you %s").format(x, y, z.mkString(", "))
    }

    repeatedParameterMethod(3, "egg", List("a delicious sandwich", "protein", "high cholesterol")) should be(
      "3 eggs can give you " + List("a delicious sandwich", "protein", "high cholesterol"))

    //if you want expanded the list
    repeatedParameterMethod(
      3,
      "egg",
        List("a delicious sandwich", "protein", "high cholesterol"):_*
    ) should be("3 eggs can give you a delicious sandwich, protein, high cholesterol")
  }

}
