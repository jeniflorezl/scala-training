import org.scalatest.FunSuite
import org.scalatest.Matchers._

class ObjectsClass extends FunSuite{

  class Movie(val name: String, val year: Short)

  object Movie{
    def academyAwardBestMoviesForYear(x: Short) = {
      x match {
        case 1930 => Some(new Movie("The inception", 1930))
        case 1931 => Some(new Movie("Cimarron", 1931))
        case 1932 => Some(new Movie("Grand Hotel", 1932))
        case _ => None
      }
    }
  }

  test("Testing movie"){
    val prueba = Movie.academyAwardBestMoviesForYear(1930) getOrElse("No found")
    Movie.academyAwardBestMoviesForYear(1930).get.name should be("The inception")
    Movie.academyAwardBestMoviesForYear(1780) getOrElse("No found") should be("No found")
  }

  //Companion object

  class Person(val name: String, private val superHeroName: String)

  object Person{
    def showMeTheSecret(x: Person) = x.superHeroName
  }

  test("Testing companion object"){
    val clark = new Person("Clark Kent", "Superman")
    val peter = new Person("Peter Park", "Spider-Man")

    Person.showMeTheSecret(clark) should be("Superman")
    Person.showMeTheSecret(peter) should be("Spider-Man")
  }

  test("companion objects"){
    class Person(val name: String, val age: Int)

    object Person{
      def newPerson(str: String) = {
        str.split("\\ ") match {
          case Array(str1, str2) => Some(new Person(str1, str2.toInt))
          case _ => None
        }
      }
    }

    val person1 = Person.newPerson("Angela 22")
    person1.get.name should be("Angela")
    person1.get.age should be(22)

    val date = raw"(\d{4})-(\d{2})-(\d{2})".r
    val regex = "2004-01-20" match {
      case date(_*) => "It's a date!"
    }
    println(regex)

    val embeddedDate = date.unanchored
    "Date: 2004-01-20 17:25:18 GMT (10 years, 28 weeks, 5 days, 17 hours and 51 minutes ago)" match {
      case embeddedDate("2004", "01", "20") => println("A Scala is born.")
    }

  }

  test("extractor objects"){
    object CustommingMessage{
      def apply(name: String) = "Hola " + name + " !!"

      def unapply(custommingMessage: String): Option[String] = {
        val array = custommingMessage.split("\\ ")
        if (array.tail.nonEmpty) Some(array(1)) else None
      }
    }

    val applyCustom = CustommingMessage("jeni")
    applyCustom should be("Hola jeni !!")
    val result = applyCustom match {
      case CustommingMessage(name) => println(name)
      case _ => println("error")
    }

    //asigning variable
    val otherWay = CustommingMessage("jeni")
    val CustommingMessage(name) = otherWay

    CustommingMessage(name) should be("Hola jeni !!")

    val nameCustommingMessage = CustommingMessage.unapply(otherWay).get

    nameCustommingMessage should be("jeni")

  }

}
