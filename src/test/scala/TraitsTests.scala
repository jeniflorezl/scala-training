import org.scalatest.FunSuite
import org.scalatest.Matchers._

class TraitsTests extends FunSuite{

  test("using traits"){
    case class Person(name: String, age: Int)

    trait Validate{
      def validateAge(person: Person) : Boolean
    }

    class MyPerson extends Validate {
      def validateAge(person: Person): Boolean = person.age match {
        case 20 => true
        case _ => false
      }
    }

    val p1 = Person("Angela", 24)
    val instancia = new MyPerson
    instancia.validateAge(p1) should be(false)
  }

  test("extends other class with trait"){
    case class Person(name: String, age: Int)

    class OurPerson

    trait Validate{
      def validateAge(person: Person) : Boolean
    }

    class MyPerson extends OurPerson with Validate {
      def validateAge(person: Person): Boolean = person.age match {
        case 25 => true
        case _ => false
      }
    }

    val p1 = Person("Angela", 25)
    val instancia = new MyPerson
    instancia.validateAge(p1) should be(true)
  }

  test("Traits are polymorphic"){
    case class Person(name: String, age: Int)

    trait Validate{
      def validateAge(person: Person) : Boolean
    }

    class MyPerson extends Validate {
      def validateAge(person: Person): Boolean = person.age match {
        case 20 => true
        case _ => false
      }
    }

    val instancia = new MyPerson
    instancia.isInstanceOf[MyPerson] should be(true)
    instancia.isInstanceOf[Validate] should be(true)
    instancia.isInstanceOf[Any] should be(true)
    instancia.isInstanceOf[AnyRef] should be(true)
  }

  test("Traits also can use self-types"){
    trait B{
      def bId = "holi"
    }

    trait A { self: B =>
      def aId = "como estas"

    }

    //does not compile
    assertDoesNotCompile("val a = new A")
    val obj = new A with B
    (obj.bId + ", " + obj.aId) should be("holi, como estas")

  }

}
