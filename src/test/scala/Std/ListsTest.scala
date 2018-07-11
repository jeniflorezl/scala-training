package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class ListsTest extends FunSuite{

  test("testing lists"){
    val list1 = List(1,2,3)
    list1.headOption should be(Some(1))
    list1.tail should be(List(2,3))
  }

  test("testing lists by position"){
    val list1 = List(1,2,3)
    list1(0) should be(1)
    list1(1) should be(2)
    list1(2) should be(3)
  }

  test("inmutables lists"){
    val list1 = List(1,2,3)
    val list2 = list1.filterNot(x => x == 3)


    list1 should be(List(1,2,3))
    list2 should be(List(1,2))

  }

  test("Operations over list"){
    val list1 = List(1,2,3)
    val list3 = list1.filter(x => x%3==0)

    val list4 = list1.filter( _%3==0)
    val list5 = list1.map(_*3)

    val list6 = list1.reduceLeft(_+_)

    val list7 = list1.fold(0)(_+_)

    val list8 = (1 to 6).toList

    list3 should be(List(3))
    list4 should be(List(3))
    list5 should be(List(3,6,9))
    list6 should be(6)
    list7 should be(6)
    list8 should be(List(1,2,3,4,5,6))

    0 :: list1 should be(List(0,1,2,3))

    list3 ::: list8 should be(List(3,1,2,3,4,5,6))
  }

  test("currying"){
    val list = List(0,1,2,3,4,5,6,7,8,9)
    val listFunc = list.foldLeft(List[Int]())_
    val r = list.foldLeft(1)((x, y) => x + y*y)

    println(r)
    val squares = listFunc((xs, x) => xs:+ x*x)
    println(squares.toString())

    val cubes = listFunc((xs, x) => xs:+ x*x*x)
    println(cubes.toString())
  }

  test("constructors of List"){
    val fruit = "apples" :: ("oranges" :: ("pears" :: Nil))
    fruit should be(List("apples", "oranges", "pears"))

    val num = 1 :: 2 :: 3 :: 4 :: Nil
    val num2 = Nil.:: (4).:: (3).:: (2).:: (1)

    num == num2 should be(true)
  }

  test("sorting lists"){

    val cond: (Int, Int) => Boolean = (x,y) => x < y
    def insert(x: Int, xs: List[Int]): List[Int] = xs match {
      case List() => x :: Nil
      case head :: tail => if (cond(x,head)) x :: head :: tail
      else head :: insert(x, tail)
    }

    def insertionSort(xs: List[Int]): List[Int] = xs match {
      case List() => List()
      case head :: tail => insert(head, insertionSort(tail))
    }

    insert(2, 1 :: 3 :: Nil) shouldBe (1 :: 2 :: 3 :: Nil)
    insert(1, 2 :: 3 :: Nil) shouldBe (1 :: 2 :: 3 :: Nil)
    insert(3, 1 :: 2 :: Nil) shouldBe (1 :: 2 :: 3 :: Nil)
  }

  test("flatMap"){
    val list = List(1,2,3)
    val result = list.flatMap(x => List(x, 2 * x, 3 * x))
    result should be(List(1,2,3,2,4,6,3,6,9))
  }

}
