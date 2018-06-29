import org.scalatest.FunSuite
import org.scalatest.Matchers._

class PatternMatchingTest extends FunSuite{
  test("testing pattern matching"){
    MatchTest1.matchTest(4) should be("many")

    val stuff = "blue"

    val myStuff = stuff match {
      case "red" => println("RED"); 1
      case "blue" => println("BLUE"); 2
      case "green" => println("GREEN"); 3
      case _ => println(stuff); 0
    }

    myStuff should be(2)
  }

  test("pattern match lists"){
    val secondElement = List(1,2,3) match {
      case x :: xs => xs.head
      case _ => 0
    }

    secondElement should be(2)

    //other way
    val secondElement2 = List(1,2,3) match {
      case x :: y :: xs => y
      case _ => 0
    }

    secondElement2 should be(2)
  }

  //only matchs with a list with exactly two elements
  test("pattern match 2"){
    val r = List(1,2,3) match {
      case x :: y :: Nil => y
      case _ => 0
    }

    r should be(0)
  }

  test("pattern 3"){
    val r = List(1, 2, 3) match {
      case x :: y :: z :: tail ⇒ tail
      case _ ⇒ 0
    }

    r == Nil should be(true)
  }


}
