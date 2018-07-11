package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._
import scala.util.matching.Regex

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

  test("case class with pattern matching"){
    abstract class Person

    case class Employee(name: String, salario: Double) extends Person

    case class Client(name: String, company: String) extends Person

    case class Provider(name: String, product: String) extends Person

    def chooseTypePerson(person: Person): String = person match {
      case Employee(name: String, salario: Double) =>
        "El empleado " + name + " gana " + salario.toInt + " pesos"
      case Client(name: String, company: String) =>
        "El cliente " + name + " viene de la empresa " + company
      case Provider(name: String, product: String) =>
        "El proveedor " + name + " nos distribuye " + product
    }

    val employee = Employee("Camilo", 1000000)
    val client = Client("Gonzalo", "Sura")
    val provider = Provider("Carlos", "Chocolates")
    chooseTypePerson(employee) should be("El empleado Camilo gana 1000000 pesos")
    chooseTypePerson(client) should be("El cliente Gonzalo viene de la empresa Sura")
    chooseTypePerson(provider) should be("El proveedor Carlos nos distribuye Chocolates")
  }

  test("case class with pattern matching Pattern Guards (condicionals)"){
    abstract class Person

    case class Employee(name: String, salario: Double) extends Person

    case class Client(name: String, company: String) extends Person

    case class Provider(name: String, product: String) extends Person

    def chooseTypePersonCondicional(person: Person, importPerson: Seq[String]): String = person match {
      case Employee(name: String, salario: Double) if importPerson.contains("employee") =>
        "El empleado " + name + " gana " + salario.toInt + " pesos"
      case Client(name: String, company: String) if importPerson.contains("client") =>
        "El cliente " + name + " viene de la empresa " + company
      case Provider(name: String, product: String) if importPerson.contains("provider") =>
        "El proveedor " + name + " nos distribuye " + product
      case other => "nothing"
    }

    val importPerson = Seq("client", "provider")
    val employee = Employee("Camilo", 1000000)
    val client = Client("Gonzalo", "Sura")
    val provider = Provider("Carlos", "Chocolates")
    chooseTypePersonCondicional(employee, importPerson) should be("nothing")
    chooseTypePersonCondicional(client, importPerson) should be("El cliente Gonzalo viene de la empresa Sura")
    chooseTypePersonCondicional(provider, importPerson) should be("El proveedor Carlos nos distribuye Chocolates")
  }

  test("matching on the type only"){
    abstract class Transport

    case class Car(time: String, cost: Double) extends Transport {
      def info = "Se demora " + time + " y cuesta " + cost.toInt
    }

    case class Subway(time: String, cost: Double) extends Transport {
      def info = "Se demora " + time + " y cuesta " + cost.toInt
    }

    case class Bus(time: String, cost: Double) extends Transport {
      def info = "Se demora " + time + " y cuesta " + cost.toInt
    }

    def conveyance(transport: Transport) = transport match {
      case c: Car => c.info
      case s: Subway => s.info
      case b: Bus => b.info
    }

    val car = conveyance(Car("1 hr", 23000)) should be("Se demora 1 hr y cuesta 23000")

  }

  test("regular expressions"){
    val patternNumber: Regex = "[0-9]".r

    patternNumber.findFirstMatchIn("uidhdkjf") match {
      case Some(_) => println("Password ok")
      case None => println("Error password")
    }

  }


}
