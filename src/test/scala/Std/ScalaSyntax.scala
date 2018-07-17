package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class ScalaSyntax extends FunSuite {

  test("declaring variables mutables and inmutables") {

    //mutables permiten hacer asignaciones
    var num = 1
    num = 2
    num should be(2)

    //specifing type
    var x: Int = 4

    x should be(4)

    //inmutables no permiten hacer asignaciones
    val num2 = 3
    assertDoesNotCompile("num2 = 4")
  }

  test("el tipo es fuerte y no debil"){
    val string = "Hola"
    assertDoesNotCompile("string = 1")
  }

  test("No se puede iniciar en null"){
    val list: Null = null
    assertDoesNotCompile("list = List(1,2,3)")
  }

  test("creating a object"){
    object Operacion{
      var x = 1
      def sum(a: Int, b: Int) = a + b + x
      def mul(a: Int, b: Int) = a * b - x
      def div(a: Int, b: Int) = a / b * x
    }

    val result = Operacion.sum(3, 4)
    result should be(8)
    assert(result == 8)
  }

  test("creating a normal class"){
    class MyClass(a: Int, b: Int){
      def f1 = a + b
      def f2 = a - b
    }

    //se crea with new keyword pasandole los argumentos de construcciòn
    val mc = new MyClass(2,6)
    mc.f1 should be(8)
    mc.f2 should be(-4)
  }

  test("A un class se le puede mutar su estado"){
    class MyClass(a: Int, b: Int){
      var r1 = 0
      def f1 = {
        r1 = r1 + a
        a + b
      }

      def f2 = {
        r1 = r1 - b
        a - b
      }
    }

    val mc = new MyClass(8,2)
    mc.r1 should be(0)
    val result1 = mc.f1
    mc.r1 should be(8)
    val result2 = mc.f2
    mc.r1 should be(6)
  }

  test("case class is a class for specific uses"){
    case class MyCaseClass(a: Int){
      def f1(b: Int) = a + b
    }

    // Se puede instanciar de forma normal
    val mc = new MyCaseClass(2)
    mc.f1(3) should be(5)

    // Se puede instanciar sin el new
    val mc1 = MyCaseClass(4)
    mc1.f1(6) should be(10)
    println(mc1)
    println(s"mcc: ${mc1}")
  }

  test("trait with only definitions"){
    trait MyTraitSum {
      def sum(a: Int, b: Int)
    }

    trait MyTraitMul {
      def mul(a: Int, b: Int)
    }

    class MyClass extends MyTraitSum with MyTraitMul{
      override def sum(a: Int, b: Int): Unit = ???

      override def mul(a: Int, b: Int): Unit = ???
    }

    assertThrows[NotImplementedError]{
      val mc = new MyClass
      mc.mul(2,3)
    }
  }

  test("traits with implementations"){
    trait MyTraitSum {
      def sum(a: Int, b: Int) = a + b
    }

    trait MyTraitMul {
      def mul(a: Int, b: Int) = a * b
    }

    class MyClass extends MyTraitSum with MyTraitMul
    val mc = new MyClass
    mc.mul(2,3) should be(6)
    mc.sum(2,3) should be(5)

  }

  test("declaring a function"){
    val list = List(1,2,3,4)
    val funcion = (x: Int) => x * 2
    def p(list: List[Int], funcion: Int => Int) = {
      val list2 = list.map(l => funcion(l))
      list2
    }

    p(list,funcion) should be(List(2,4,6,8))

  }

  test("pattern matching"){
    case class Estudiante(nombre: String, edad: Int)
    case class Profesor(nombre: String)
    case class Curso(nombre: String, p: Profesor, estudiantes: List[Estudiante])

    val c1 = Curso("Matematicas", Profesor("Julio Garcia"),
      List(Estudiante("Juan", 21),
        Estudiante("Pablo", 23),
        Estudiante("Santiago", 18),
        Estudiante("Camilo", 19),
        Estudiante("Gonzalo", 25)))

    val c2 = Curso

    c1 match {
      case x: Curso if x.p.nombre != "Julio Garcia" => {
        assert(x.nombre == "Matematicas")
        assert(x.p == Profesor("Julio Garcia"))
      }
      case Curso(n,p,e) if p.nombre == "Julio Garcia" => {
        println("entro")
        assert(n == "Matematicas")
        assert(p == Profesor("Julio Garcia"))
        assert(e == List(Estudiante("Juan", 21),
          Estudiante("Pablo", 23),
          Estudiante("Santiago", 18),
          Estudiante("Camilo", 19),
          Estudiante("Gonzalo", 25)))
      }
    }
  }

  test("verify unapply"){
    class Estudiante(nombre: String)

    object Estudiante{
      def unapply(arg: Estudiante): Option[String] = Some("NOMBRE")
    }

    new Estudiante("Carlos") match {
      case Estudiante(n) => {
        assert(n == "NOMBRE")
      }
    }
  }


}
