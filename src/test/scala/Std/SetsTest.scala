package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.immutable.{BitSet, ListSet, SortedSet}

class SetsTest extends FunSuite{

  //no dupplicate elements
  test("creating a set"){
    val mySet = Set("Jeniffer", "Angela", "Camila")
    mySet.size should be(3)
  }

  //subsets
  test("validate subsets"){
    val mySet1 = Set("Jeniffer", "Alejandra", "Camila")
    val mySet2 = Set("Jeniffer", "Alejandra")
    val mySet3 = Set("Jeniffer", "Carlos")

    mySet2 subsetOf mySet1 should be(true)
    mySet3 subsetOf mySet1 should be(false)
  }

  //diffrence
  test("difference") {
    val mySet=Set("Hola", "Adios", "Bye", "Hello")
    val mySet2 = Set("Hola", "Bye")
    val difference = mySet diff mySet2

    difference.equals(Set("Adios","Hello"))
  }

  test("creatin a Set"){
    val set1 = SortedSet("Hola", "Adios", "Bye")
    set1 should be(Set("Adios", "Bye", "Hola"))
    set1.headOption should be(Some("Adios"))
  }

  test("ListsSet"){
    val s = ListSet.empty[Int]
    val r = s + 1 + 4 + 3 + 2
    println(r)
    print(r + 7)
  }

  test("BitSet"){
    val s = BitSet.empty
    val r = s + 2 + 1 + 3 + 0
    println(r)
  }

  test("apply in set"){
    val s = Set(1,2,3,4)
    val s2 = Set(5,6,7,8)
    val res = s.apply(2)
    res should be(true)
  }

  test("transpose"){
    val list = List(Set(1,2,3), Set(4,5,6))
    list.transpose should be(List(List(1,4), List(2,5), List(3,6)))
  }
}
