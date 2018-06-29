import org.scalatest.FunSuite

class helloWorld extends FunSuite {


  test("Testing hello world"){
    assertResult("Hello World!"){
      HelloWorld.main()
    }
  }

}
