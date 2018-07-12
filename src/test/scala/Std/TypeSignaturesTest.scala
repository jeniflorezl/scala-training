package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class TypeSignaturesTest extends FunSuite{

  test("scala will infer the type"){
    val z = "Do" :: "Re" :: "Mi" :: "Fa" :: "Sol" :: Nil
    z should be(List("Do", "Re", "Mi", "Fa", "Sol"))
  }

  test("a trait can be declared contaning a type"){
    trait Randomizer[A]{
      def draw(): A
    }

    class IntRandomizer extends Randomizer[Int]{
      def draw = {
        import util.Random
        Random.nextInt()
      }
    }

    val intRand = new IntRandomizer
    (intRand.draw < Int.MaxValue) should be(true)
  }

  test("class meta-information can be retrieved by class name by using classOf"){
    classOf[String].getCanonicalName should be("java.lang.String")
    classOf[String].getSimpleName should be("String")

    //from an object
    val z = "zoom"
    z.isInstanceOf[String] should be(true)
    z.getClass.getCanonicalName should be("java.lang.String")
    z.getClass.getSimpleName should be("String")
  }

  test("isInstanceOf[className] is used to determine if an object reference " +
    "is an instance of given class"){
    trait Randomizer[A]{
      def draw(): A
    }

    class IntRandomizer extends Randomizer[Int]{
      def draw = {
        import util.Random
        Random.nextInt()
      }
    }

    val intRand = new IntRandomizer
    intRand.isInstanceOf[IntRandomizer] should be(true)
    intRand.isInstanceOf[Randomizer[Int]] should be(true)
    intRand.draw.isInstanceOf[Int] should be(true)
  }

}
