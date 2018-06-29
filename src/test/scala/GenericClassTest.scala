import org.scalatest.FunSuite

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

}
