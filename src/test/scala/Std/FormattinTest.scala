package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class FormattinTest extends FunSuite{

  test("Formattin"){
    val s = "Hello World"
    "Application %s".format(s) should be("Application Hello World")

  }

  test("character literals") {
    val a = 'a'
    val b = 'B'
    println("%c".format(a))
    println("%c".format(b))

    val c = 'a' //unicode for a
    val d = '\141' //octal for a
    val e = '\"'
    val f = '\\'

    println("%c".format(c))
    println("%c".format(d))
    println("%c".format(e))
    println("%c".format(f))

    val j = 190
    println("%d bottles of beer on the wall" format j - 100)
  }

}
