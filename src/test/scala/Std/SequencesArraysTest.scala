package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class SequencesArraysTest extends FunSuite{

  test("creating an array from a list"){
    val list = List(1,2,3)
    val array = list.toArray
    array should be(Array(1,2,3))

    //creatin Array
    val array1 = Array(1,2,3,4)
    array1 should be(Array(1,2,3,4))
  }

  test("any sequence can be convert to list"){
    val array = Array(2,4,6,8)
    val s = array.toSeq
    val list = s.toList
    list should be(List(2,4,6,8))
  }

  test("create sequence from a for loop"){
    val forloop = for(i <- 1 to 4 ) yield i
    forloop.toList should be(List(1,2,3,4))
  }

  test("create sequence from a for loop with filter"){
    val s = for(i <- 1 to 10 if i % 3 == 0) yield i
    s.toList should be(List(3,6,9))
  }

  test("filter any sequence base on a predicate"){
    val s = Seq("hello", "Yes", "bye")
    val filtered = s.filter(_.startsWith("Y"))
    filtered should be(Seq("Yes"))
  }

  test("filtered arrays"){
    val array = Array("hello", "Yes", "bye")
    val filtered = array.filter(_.startsWith("Y"))
    filtered should be(Array("Yes"))
  }

  test("map to values with a function"){
    val s = Seq("hello", "Yes", "bye")
    val changes = s.map(s => s.replace(s,s.toUpperCase))
    changes should be(Seq("HELLO", "YES", "BYE"))
  }

}
