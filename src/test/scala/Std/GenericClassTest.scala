package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class GenericClassTest extends FunSuite{

  test("testing generic class"){
    val stack = new GenericClass[Int]
    stack.push(1)
    stack.push(2)
    //println(stack.pop)
    //println(stack.pop)
    assert(stack.pop()==2)
    assert(stack.pop()==1)
  }

  test("other example"){
    trait Fruit
    case class Apple(name: String) extends Fruit
    case class Banana(name: String) extends Fruit

    val stack = new GenericClass[Fruit]
    val apple = Apple("apple")
    val banana = Banana("banana")
    stack.push(apple)
    stack.push(banana)

    stack.retrieve should be(List(Banana("banana"), Apple("apple")))
  }

}
