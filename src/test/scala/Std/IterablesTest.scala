package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers.{be, _}

class IterablesTest  extends FunSuite{
  test("iterator with list"){
    val list = List(2,5,6,7,7)
    val it = list.iterator
    if (it.hasNext){
      it.next() should be(2)
    }

  }

  test("return un fixed-size iterable"){
    val list = List(2,5,6,7,2,3)
    val it = list grouped 3
    it.next() should be(List(2,5,6))
    it.next() should be(List(7,2,3))
  }

  test("return a sliding window for travel the iterable"){

    //sliding window, va recorriendo mostrando de a determinado tamaño
    val list = List(2,5,6,7,2,3)
    val it2 = list sliding 3
    it2.next() should be(List(2,5,6))
    it2.next() should be(List(5,6,7))

    //cantidad y posicion
    val it3 = list sliding (3,3)
    it3.next() should be(List(2,5,6))
    it3.next() should be(List(7,2,3))
  }

  test("take 3 lastest elements"){
    //toma los ultimos elementos del iterador
    val list = List(2,5,6,7,2,3)
    val it4 = list takeRight 3
    it4 should be(List(7,2,3))
  }

  test("dropRight, drop element at right"){
    //borra los ultimos elementos del iterador
    val list = List(2,5,6,7,2,3)
    val it4 = list dropRight  3
    it4 should be(List(2,5,6))
  }

  test("zip stitch two iterables"){
    val list = List(3,5,9)
    val list2 = List("Cami", "Sofi", "Lau")
    list zip list2 should be(List((3, "Cami"), (5, "Sofi"), (9, "Lau")))

    //works until it can be paired
    val list3 = List(3,5)
    val list4 = List("Cami", "Sofi", "Lau")
    list3 zip list4 should be(List((3, "Cami"), (5, "Sofi")))

    list3 zipAll (list4, 2, "Rui") should be(List((3,"Cami"),(5,"Sofi"),(2,"Lau")))
  }

  test("zipWithIndex"){
    val list = List(1,2,3)
    list.zipWithIndex should be(List((1,0), (2,1), (3,2)))
  }

  test("same elements"){
    //return true only if two iterables have the same elements and same order
    val list = List(3,4,5)
    val list1 = List(3,4,5)

    list sameElements list1 should be(true)

    val list2 = List(5,4,3)

    list sameElements list2 should be(false)

    val set3= Set(1,2,3,5,6,7,8,4)
    val set4 = Set(4,2,3,5,6,7,8,1)

    set3 sameElements set4 should be(true)
  }
}
