import org.scalatest.FunSuite
import org.scalatest.Matchers._

class InfixTypesPostfixTests extends FunSuite{

  test("infix of type op[T1][T2]"){
    case class Person(age: Int)

    class Ages[P1, P2](val p1: P1, val p2: P2)

    def operator(persons: Person Ages Person)={
      "la persona p1 tiene "+persons.p1.age + " años y la persona p2 " + persons.p2.age
    }

    val person = Person(22)
    val person2 = Person(25)
    operator(new Ages(person, person2)) should be("la persona p1 tiene 22 años y la persona p2 25")

  }

  test("creating a infix operator for use our infix type"){
    case class Person(age: Int){
      def ages(person: Person) = new Ages(this, person)
    }

    class Ages[P1, P2](val p1: P1, val p2: P2)

    def operator(persons: Person Ages Person)={
      "la persona p1 tiene "+persons.p1.age + " años y la persona p2 " + persons.p2.age
    }

    val person = Person(22)
    val person2 = Person(25)
    operator(person ages person2) should be("la persona p1 tiene 22 años y la persona p2 25")

  }

}
