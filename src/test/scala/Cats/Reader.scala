package Cats

import cats.data.Reader
import org.scalatest.Matchers.be

class Reader {
  test("Test reader"){
    val upper = Reader((text: String) => text.toUpperCase)
    val greet = Reader((name: String) => s"Hello $name")

    upper.run("hola") should be("HOLA")
  }

  test("composed"){
    val cube = (x : Int) => x * x * x
    val cubeSum = cube andThen(z => z + 3)
    cubeSum(5) should be(128)

    //with reader
    val cubeReader = Reader((x: Int) => x * x * x)
    val cubeSumReader = cubeReader map (z => z + 3)

    cubeSumReader(5) should be(128)
  }

  test("injecting dependency"){
    trait AuthService{
      def isLogged(name: String): Boolean
    }

    class AuthServiceChar3 extends AuthService{
      override def isLogged(name: String): Boolean = name.length == 3
    }

    class AuthServiceChar5 extends AuthService{
      override def isLogged(name: String): Boolean = name.length == 5
    }

    trait UserService{
      def greet(name: String, isLogged: Boolean) :String
    }

    class UserServiceDefaultUser extends UserService{
      override def greet(name: String, isLogged: Boolean): String = {
        val actualName = if (isLogged) name else "User"
        s"Hello $actualName"
      }
    }

    class UserServiceNoDefault extends UserService{
      override def greet(name: String, isLogged: Boolean): String = {
        if (isLogged) s"Hello $name" else "No authorization"
      }
    }

    case class Environment(userName: String, userService: UserService, authService: AuthService)

    //readers

    def isLoggedUser= Reader[Environment, Boolean] { env =>
      env.authService.isLogged(env.userName)
    }

    def greetUser(logged: Boolean) = Reader[Environment, String]{ env =>
      env.userService.greet(env.userName, logged)
      //Reader((env : Environment) => env.userService.greet(env.userName, logged))
    }

    val resultR = for {
      logged <- isLoggedUser
      greeting <- greetUser(logged)
    } yield greeting

    val environment1 = Environment("Joe", new UserServiceDefaultUser, new AuthServiceChar3)
    println(resultR.run(environment1))

    val environment2 = Environment("Joe", new UserServiceNoDefault, new AuthServiceChar5)
    println(resultR.run(environment2))
  }
}
