import org.scalatest.FunSuite
import org.scalatest.Matchers._

class CaseClassesTest extends FunSuite{

  abstract class Term
  case class Var(name: String) extends Term
  case class Fun(arg: String, body: Term) extends Term
  case class App(f: Term, v: Term) extends Term

  test("case classes"){
    val x1 =  Var("x")
    println(x1)
  }

  case class Person(first: String, last: String)
  test("HashCode method"){
    val p1 = new Person("Fred", "Jones")
    val p2 = new Person("Shaggy", "Rogers")
    val p3 = new Person("Fred", "Jones")

    (p1.hashCode() == p2.hashCode()) should be(false)
    (p1.hashCode() == p3.hashCode()) should be(true)
  }

  case class Dog(var name: String, breed: String)

  test("case classes toString"){
    val d1 = Dog("Scooby", "Doberman")
    d1.toString should be("Dog(Scooby,Doberman)")

    d1.name = "Jeniffer"

    d1.name should be("Jeniffer")

  }

  test("safer alternatives for altering case classes"){
    val d1 = Dog("Scooby", "Doberman")

    val d2 = d1.copy(name = "Scooby Doo", breed = "Dob")

    d2.name should be("Scooby Doo")
    d2.breed should be("Dob")
  }

  case class Person2(first: String, last: String, age: Int = 0, ssn: String = "")

  test("case classes can have default and named parameters"){
    val p1 = Person2("Fred", "Jones", 23, "111-22-3333")
    val p2 = Person2("Samantha", "Jones")
    val p3 = Person2(last = "Jones", first = "Fred", ssn = "111-22-3333")
    val p4 = p3.copy(age = 23)

    p1.first should be("Fred")
    p1.last should be("Jones")
    p1.age should be(23)
    p1.ssn should be("111-22-3333")

    p2.first should be("Samantha")
    p2.last should be("Jones")
    p2.age should be(0)
    p2.ssn should be("")

    p3.first should be("Fred")
    p3.last should be("Jones")
    p3.age should be(0)
    p3.ssn should be("111-22-3333")

    (p1 == p4) should be(true)
  }

}
