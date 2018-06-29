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

}
