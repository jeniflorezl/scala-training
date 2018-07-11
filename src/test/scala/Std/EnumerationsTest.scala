package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class EnumerationsTest extends FunSuite{

  test("creating enumerations"){
    object Planets extends Enumeration{
      val Mercury = Value
      val Venus = Value
      val Earth = Value
      val Mars = Value
      val Jupiter = Value
      val Saturn = Value
      val Uranus = Value
      val Neptune = Value
      val Pluto = Value
    }

    Planets.Mercury.id should be(0)
    Planets.Mercury.toString should be("Mercury")
    (Planets.Earth == Planets.Earth) should be(true)
    (Planets.Neptune == Planets.Jupiter) should be(false)
  }

  test("defining value and string"){
    object Colors extends Enumeration{
      val YELLOW = Value(1, "Yellow")
      val BLUE = Value(2, "Blue")
      val RED = Value(3, "Red")
    }

    Colors.YELLOW.id should be(1)
    Colors.YELLOW.toString should be("Yellow")
    (Colors.BLUE == Colors.BLUE) should be(true)
    (Colors.RED == Colors.YELLOW) should be(false)
  }

  test("declaring in a one line"){
    object Colors extends Enumeration{
      val YELLOW, BLUE, RED = Value
    }
  }

  test("declaring only string"){
    object Colors extends Enumeration{
      val YELLOW = Value("Yellow")
      val BLUE = Value("Blue")
      val RED = Value("Red")
    }
    Colors.YELLOW.id should be(0)
    Colors.YELLOW.toString should be("Yellow")
    (Colors.BLUE == Colors.BLUE) should be(true)
    (Colors.RED == Colors.YELLOW) should be(false)
  }

  test("extends the Enumeration by extending the Value class"){
    object Planets extends Enumeration{
      val G = 6.67300E-11

      class PlanetValue(val i: Int, val name: String, val mass: Double, val radius: Double)
        extends Val(i: Int, name: String){

        def surfaceGravity = G * mass / (radius * radius)

        def surfaceWeigth(otherMass: Double) = otherMass * surfaceGravity

        def compare(that: PlanetValue) = this.i - that.i
      }

      val Mercury = new PlanetValue(0, "Mercury", 3.303e+23, 2.4397e6)
      val Venus = new PlanetValue(1, "Venus", 4.869e+24, 6.0518e6)
      val Earth = new PlanetValue(2, "Earth", 5.976e+24, 6.37814e6)
      val Mars = new PlanetValue(3, "Mars", 6.421e+23, 3.3972e6)
      val Jupiter = new PlanetValue(4, "Jupiter", 1.9e+27, 7.1492e7)
      val Saturn = new PlanetValue(5, "Saturn", 5.688e+26, 6.0268e7)
      val Uranus = new PlanetValue(6, "Uranus", 8.686e+25, 2.5559e7)
      val Neptune = new PlanetValue(7, "Neptune", 1.024e+26, 2.4746e7)
      val Pluto = new PlanetValue(8, "Pluto", 1.27e+22, 1.137e6)

      Planets.Mercury.mass should be(3.303e+23)
      Planets.Mercury.radius should be(2.4397e6)
      Planets.Jupiter.compare(Planets.Pluto) should be(-4)

    }
  }

}
