import org.scalatest.FunSuite
import org.scalatest.Matchers._

class PartialFunctionsTest extends FunSuite{

  test("partial functions"){
    //the apply and isDefineAt are created automatically
    val doubleEvens: PartialFunction[Int, Int] = {
      case x if (x%2) == 0 => x*2
    }

    val trippleOdds: PartialFunction[Int, Int] = {
      case x if (x%2) != 0 => x*3
    }

    val whatToDo = doubleEvens orElse trippleOdds

    whatToDo(3) should be(9)
    whatToDo(4) should be(8)
  }

  //andThen
  test("partial functions add then"){
    //the apply and isDefineAt are created automatically
    val doubleEvens: PartialFunction[Int, Int] = {
      case x if (x%2) == 0 => x*2
    }

    val trippleOdds: PartialFunction[Int, Int] = {
      case x if (x%2) != 0 => x*3
    }

    val addFive = (x: Int) => x + 5


    val whatToDo = doubleEvens orElse trippleOdds andThen addFive

    whatToDo(3) should be(14)
    whatToDo(4) should be(13)
  }

  //andThen orElse
  test("partial functions addthen orElse"){
    //the apply and isDefineAt are created automatically
    val doubleEvens: PartialFunction[Int, Int] = {
      case x if (x%2) == 0 => x*2
    }

    val trippleOdds: PartialFunction[Int, Int] = {
      case x if (x%2) != 0 => x*3
    }

    val printEven: PartialFunction[Int, String] = {
      case x if (x % 2 == 0) => "Even"
    }

    val printOdd: PartialFunction[Int, String] = {
      case x if (x % 2 != 0) => "Odd"
    }


    val whatToDo = doubleEvens orElse trippleOdds andThen (printEven orElse printOdd)

    whatToDo(3) should be("Odd")
    whatToDo(4) should be("Even")
  }



}
