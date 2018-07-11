package Std

import org.scalatest.FunSuite


class TreeTest extends FunSuite{
  abstract class Tree
  case class Sum(l: Tree, r: Tree) extends Tree
  case class Var(n: String) extends Tree
  case class Const(v: Int) extends Tree

  object Cal{
    type Environment = String => Int

    def eval(t: Tree, env: Environment): Int = t match {
      case Sum(l, r) => eval(l, env) + eval(r, env)
      case Var(n) => env(n)
      case Const(v) => v
    }

    def derive(t: Tree, v: String): Tree = t match {
      case Sum(l, r) => Sum(derive(l, v), derive(r,v))
      case Var(n) if (v == n) => Const(1)
      case _ => Const(0)
    }
  }

  test("test tree"){

    val exp: Tree = Sum(Sum(Var("x"), Var("x")), Sum(Const(7), Var("y")))
    val env: Cal.Environment = { case "x" => 5 case "y" => 7}
    println("Expression: " + exp)
    println("Evaluation with x=5, y=7: " + Cal.eval(exp, env))
    println("Derivative relative to x:\n " + Cal.derive(exp, "x"))
    println("Derivative relative to y:\n " + Cal.derive(exp, "y"))
  }

}
