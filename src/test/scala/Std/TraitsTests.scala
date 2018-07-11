package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.mutable.ArrayBuffer

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

  test("traits 2 "){
    trait Operator{
      def sum(a: Int, b: Int): Int
    }

    trait Operator2{
      def mul(a: Int, b: Int): Int = a * b
    }

    class Operacion extends Operator{
      override def sum(a: Int, b: Int): Int = a + b
    }

    class OperacionModify(private var x: Int, private var y: Int) extends Operator{
      override def sum(a: Int, b: Int): Int = a + b - (x + y)
    }

    class OperacionNew extends Operator2

    val result = new Operacion
    result.sum(3,4) should be(7)

    val result2 = new OperacionModify(7,8)
    result2.sum(3,4) should be(-8)

    val result3 = new OperacionNew
    result3.mul(3,2) should be(6)

  }

  test("tratis 3"){
    trait Partner{
      val name: String
      val age: Int
    }

    class Person2(val name: String, val age: Int) extends Partner

    val person1 = new Person2("Angela", 24)
    val person2 = new Person2("Carlos", 22)

    val partners = ArrayBuffer.empty[Partner]
    partners.append(person1)
    partners.append(person2)
    println(partners)
    partners.foreach(partner => println(partner.name + " " + partner.age))
  }

  test("mixins 2"){
    abstract class Iterator{
      type T
      def hasNext: Boolean
      def next(): T
    }

    class StringIterator(s: String) extends Iterator{
      type T = Char
      private var contador = 0
      def hasNext = contador < s.length
      def next() = {
        val ch = s charAt(contador)
        contador += 1
        ch
      }
    }

    trait RichIterator extends Iterator{
      def foreach(f: T => Unit) = while(hasNext) f(next())
    }

    class StringRichIterator extends StringIterator("Scala") with RichIterator
    val richString = new StringRichIterator
    richString foreach println

  }

}
