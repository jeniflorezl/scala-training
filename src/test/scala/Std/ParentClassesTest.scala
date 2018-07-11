package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class ParentClassesTest extends FunSuite{

  class Soldier(val firstName: String, val lastName: String)
  class Pilot(override val firstName: String, override val lastName: String, val squadron: Long) extends
    Soldier(firstName, lastName)

  test("class hierarchy is linear, a class only extend from one parent class"){

    val pilot = new Pilot("Jhon", "Gaviria", 256)
    pilot.firstName should be("Jhon")
    pilot.lastName should be("Gaviria")
  }

  test("a class that extends from another is polymorphic"){
    val pilot = new Pilot("Jhon", "Gaviria", 256)
    val soldier = pilot
    soldier.firstName should be("Jhon")
    soldier.lastName should be("Gaviria")
  }

  test("abstract class can not be instantiated and only inherited"){
    abstract class Soldier(val firstName: String, val lastName: String)
    assertDoesNotCompile("new Soldier(Jhon, Gaviria)")
  }

  test("a class can be placed inside an abstract class"){
    abstract class Soldier(val firstName: String, val lastName: String){
      class Cath(val number: Long){}
    }

    class Pilot(override val firstName: String, override val lastName: String, val squadron: Long) extends
      Soldier(firstName, lastName)

    val pilot = new Pilot("Jhon", "Gaviria", 256)
    val catchNo = new pilot.Cath(22)
    catchNo.number should be(22)
  }

}
