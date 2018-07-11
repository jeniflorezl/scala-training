package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class ExtractorsTest extends FunSuite{

  test("extractor object"){
    object Twice{
      def apply(n: Int) = n * 2
      def unapply(z: Int): Option[Int] = if (z % 2 == 0) Some(z / 2) else None
    }

    val twice = Twice(10)
    val result = twice match {
      case Twice(n) => n
    }
    result should be(10)

  }

  test("many unapplys"){
    class Car(val make: String, val model: String, val year: Short, val toSpedd: Short)
    class Employee(val firstName: String, val middleName: Option[String], val lastName: String)

    object Tokenizer{
      def unapply(x: Car) = Some(x.make, x.model, x.year, x.toSpedd)
      def unapply(x: Employee) = Some(x.firstName, x.lastName)
    }

    val result = new Employee("Kurt", None, "Vonnegut") match {
      case Tokenizer(c,d) => "c: %s, d: %s".format(c,d)
      case _ => "Not found"
    }

    println(result)
  }

  test("unapply for pattern matching"){
    class Employee(
      val firstName: String,
      val middleName: Option[String],
      val lastName: String
    )

    object Employee {
      def unapply(x: Employee) = Some(x.lastName, x.middleName, x.firstName)
    }

    val singri = new Employee("Singri", None, "Keerthi")

    val result = singri match {
      case Employee("Singri", None, x) => "Yay, Singri %s! with no middle name!".format(x)
      case Employee("Singri", Some(x), _) => "Yay, Singri with a middle name of %s".format(x)
      case _ => "I don't care, going on break"
    }

    println(result)
  }

  test("custom extractors"){
    class Employee(
                    val firstName: String,
                    val middleName: Option[String],
                    val lastName: String
                  )

    object Employee {
      //factory methods, extractors, apply
      //Extractor: Create tokens that represent your object
      def unapply(x: Employee) =
        Some(x.lastName, x.middleName, x.firstName)
    }

    val singri = new Employee("Singri", None, "Keerthi")

    val Employee(a, b, c) = singri

    a should be("Keerthi")
    b should be(None)
    c should be("Singri")
  }

  test("instantiated classes"){
    class Car(val make: String, val model: String, val year: Short, val topSpeed: Short) {
      def unapply(x: Car) = Some(x.make, x.model)
    }

    val camaro = new Car("Chevy", "Camaro", 1978, 122)

    val result = camaro match {
      case camaro(make, model) ⇒ "make: %s, model: %s".format(make, model)
      case _ ⇒ "unknown"
    }

    result should be("make: Chevy, model: Camaro")
  }

}
