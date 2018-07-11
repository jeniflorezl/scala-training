package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.immutable.Stream.cons

class TraversablesTest extends FunSuite{
  test("traversables"){
    val list = List(1,2,3)
    val set = Set(4,5,6)
    val result = list ++ set
    val result2 = set ++ list
    result should be(List(1,2,3,4,5,6))
    result2 should be(Set(4,5,6,1,2,3))
    println(result.size)
    println(result2.size)

    val result3 = set.map(_*4)
    result3.lastOption should be(Some(24))
  }

  test("pack all into a single traversable"){
    val list = List(List(1), List(2,3,4), List(4,6,7), List(9,2,4))
    list.flatten should be(List(1,2,3,4,4,6,7,9,2,4))
  }

  test("flatmap seems like flatten"){
    val list = List(List(1), List(2,3,4), List(4,6,7), List(9,2,4))
    val result = list.flatMap(_.map(_*4))
    result should be(List(4,8,12,16,16,24,28,36,8,16))
  }

  test("flatmap de optiones, filter out all Nones but keep the Somes"){
    val list = List(2,3,4,5,6,7,8)
    val result = list.flatMap(num => if (num % 2 == 0) Some(num) else None)
    result should be(List(2,4,6,8))
  }

  test("collect apply a partial function to all elements and return a different collection"){
    val list = List(21,4,6,2,7,8,5,4,7,8)
    val result = list.collect{
      case num: Int if (num % 2 != 0) => num * 2
      case num => num
    }

    result should be(List(42,4,6,2,14,8,10,4,14,8))
  }

  test("two case fragments can be chained to create more robust result"){
    val list = List(1,2,3,4,5,6)
    val partialFunction: PartialFunction[Int,Int] = {
      case x: Int if (x % 2 == 0) => x * 3
    }

    val partialFunction2: PartialFunction[Int, Int] = {
      case x: Int if (x % 2 != 0) => x * 5
    }

    val result = list.collect(partialFunction orElse partialFunction2)
    result should be(List(5, 6, 15, 12, 25, 18))
  }

  test("foreach no return"){
    val list = List(2,3,4,4,5,6)
    list.foreach(println(_))
    list should be(List(2,3,4,4,5,6))
  }

  test("ToArray convert to Array"){
    val set = Set(1,2,3)
    val array = set.toArray
    array.isInstanceOf[Array[Int]] should be(true)

    //List
    val list = List(1,2,3,4)
    val array2 = list.toArray
    array2.isInstanceOf[Array[Int]] should be(true)

    //From Set to List
    val list2 = set.toList
    list2 should be(List(1,2,3))

    list2.toList eq list2 should be(true)
  }

  test("toIterable"){
    val set = Set(4,6,8)
    val result = set.toIterable
    result.isInstanceOf[Iterable[_]] should be(true)
  }

  test("Seq is a ordered iterable"){
    val set = Set(3,5,7)
    val sequencia = set.toSeq
    println(sequencia.apply(2))
    sequencia.isInstanceOf[Seq[_]] should be(true)

    val result = set.toIndexedSeq
    println(result(0))
  }

  test("toStream"){
    val list = List(1,2,3,4)
    val stream = list.toStream
    val result = (stream map(x => x * 2)).filter(y => y < 6)
    result should be(Stream(2,4))
  }

  test("toSeq collection of unordered and unique values"){
    val list = List(1,2,3,4)
    val set = list.toSeq
    set should be(Seq(1,2,3,4))
  }

  test("toMap, convert to map"){
    val list = List("Sara" -> 23, "Gonzalo" -> 22)
    val map = list.toMap
    map should be(Map("Sara" -> 23, "Gonzalo" -> 22))
    map.isInstanceOf[Map[_,_]] should be(true)
    val list2 = List(1,2,3,4)
    assertDoesNotCompile("list2.toMap")

    //Set
    val set = Set("Carros" -> 22, "Motos" -> 15, "Buses" -> 30)
    set.isEmpty should be(false)

    val map2 = set.toMap
    map2.nonEmpty should be(true)
    map2.size should be(3)
    map2 should be(Map("Carros" -> 22, "Motos" -> 15, "Buses" -> 30))
  }

  test("hasDefiniteSize return true if only the traversable hace definite size"){
    val map = Map("Sara" -> 23, "Gonzalo" -> 22)
    map.hasDefiniteSize should be(true)

    val stream = cons(0, cons(1, Stream.empty))
    println(stream)
    stream.hasDefiniteSize should be(false)
  }

  test("head return the first element in a traversable"){
    val list = List(1,2,3)
    list.head should be(1)
  }

  test("HeadOption return the first element in a Option with Some and None"){
    val list = List(1,2,3)
    list.headOption should be(Some(1))
  }

  test("last return the last element of a colllection"){
    val list = List(1,2,3)
    list.last should be(3)
  }

  test("lastOption return the last element of a colllection in a Option with Some and None"){
    val list = List(1,2,3)
    list.lastOption should be(Some(3))
  }

  test("find return the first element matchs with the predicate in a Option"){
    val list = List(1,2,3,4,5,6,7)
    list.find(x => x % 2 == 0) should be(Some(2))
  }

  test("tail return the rest of a list without the head"){
    val list = List(1,2,3,4,5,6,7)
    list.tail should be(List(2,3,4,5,6,7))
  }

  test("init return the rest of a list without the last"){
    val list = List(1,2,3,4,5,6,7)
    list.init should be(List(1,2,3,4,5,6))
  }

  test("given a from index and to index, slice returns the part of collection including from and excluding to"){
    val list = List(1,2,3,4,5,6,7)
    list.slice(2,5) should be(List(3,4,5))
  }

  test("take first number of elements given"){
    val list = List(1,2,3,4,5,6,7)
    list take 3 should be(List(1,2,3))
  }

  test("takes is used often with streams"){
    def streamer(v:Int) : Stream[Int] = cons(v, streamer(v+1))
    val a = streamer(2)
    (a take 6 toList) should be(List(2,3,4,5,6,7))
  }

  test("drop will take the rest of Traversable except the number of elements given"){
    def streamer(v:Int) : Stream[Int] = cons(v, streamer(v+1))
    val a = streamer(2)
    ((a drop 6) take 3 toList) should be(List(8,9,10))
  }

  test("takeWhile will continually accumulate elements until predicate is no loger satisfied"){
    val list = List(1,2,3,4,5)
    val result = list.takeWhile(x => x < 10)
    result should be(List(1,2,3,4,5))
  }

  test("dropWhile will continually drop elements until predicate is no loger satisfied"){
    val list = List(1,2,3,4,11)
    val result = list.dropWhile(x => x < 10)
    result should be(List(11))
  }

  test("filter returns elements that satisfied the condicion"){
    val array = Array(1,2,3,4)
    array.filter(x => x > 3) should be(Array(4))

    //filterNot opposite to filter
    array.filterNot(x => x > 3) should be(Array(1,2,3))
  }

  test("splitAt will split a Traversable at position, returning 2 product Tuple.splitA"){
    val array = Array(23,5,53,5,56,7,83,44,33,67)
    val result = array splitAt 3
    result._1 should be(Array(23,5,53))
    result._2.foreach(println)
    //other way
    println(array take 3, array drop 3)
  }

  test("span will split a Traversable according to a predicate"){
    val array = Array(23,5,210,5,56,7,101,44,100,67)
    val result = array span (_ < 100)
    result._1.foreach(println)
    result._1 should be(Array(23,5))
    result._2.foreach(println)
    result._2 should be(Array(210,5,56,7,101,44,100,67))
    //other way
    println(array takeWhile (_ < 100), array dropWhile (_ < 100))
  }

  test("partition will split a Traversable according to a predicate. Left-hand side contains the elements" +
    "satisfied by the predicated whereas the right hand side contains the rest of elements"){
    val array = Array(23,5,210,5,56,7,101,44,100,67)
    val result = array partition (_ < 100)
    result._1 should be(Array(23,5,5,56,7,44,67))
    result._2 should be(Array(210,101,100))
  }

  test("groupBy "){
    val array = Array(23,5,210,5,56,7,101,44,100,67)

    val oddAndSmallPartial: PartialFunction[Int, String] = {
      case x: Int if x % 2 != 0 && x < 100 => "Odd and less than 100";
    }

    val evenAndSmallPartial: PartialFunction[Int, String] = {
      case x: Int if x != 0 && x % 2 == 0 && x < 100 => "Even and less than 100"
    }

    val negativePartial: PartialFunction[Int, String] = {
      case x: Int if x < 0 => "Negative number"
    }

    val largeNumber: PartialFunction[Int, String] = {
      case x: Int if x >= 100 => "Large number"
    }

    val result = array groupBy {
      oddAndSmallPartial orElse
      evenAndSmallPartial orElse
      negativePartial orElse
      largeNumber
    }

    result("Even and less than 100") should be(Array(56,44))
    result("Odd and less than 100") should be(Array(23,5,5,7,67))

  }

  test("forall determinates if a predicate is true for all elements"){
    val list = List(1,2,3,4)
    list forall(_<5) should be(true)
  }

  test("forall determinates if a predicate is true for some elements"){
    val list = List(1,2,3,4)
    list exists (_<5) should be(true)
  }

  test("count the number of elements that satisfied a predicated"){
    val list = List(1,2,3,4)
    list count (_<5) should be(4)
  }

  test("/: or foldLeft"){
    val list = List(5, 4, 3, 2, 1)
    val result = (0 /: list) { (`running total`, `next element`) ⇒
      `running total` - `next element`
    }
    println(result)
    result should be(-15)

    val result2 = list.foldLeft(0) { (`running total`, `next element`) ⇒
      `running total` - `next element`
    }
    println(result2)
    result2 should be(-15)

    val result3 = (0 /: list)(_ - _) //Short hand
    result3 should be(-15)

    val result4 = list.foldLeft(0)(_ - _)
    result4 should be(-15)

    (((((0 - 5) - 4) - 3) - 2) - 1) should be(-15)
  }

  test(" ':\' foldRight"){
    val list = List(5, 4, 3, 2, 1)
    val result = (list :\ 0) { (`next element`, `running total`) ⇒
      `next element` -`running total`
    }
    println(result)
    result should be(3)

    val result2 = list.foldRight(0) { (`next element`, `running total`) ⇒
      `next element` - `running total`
    }
    println(result2)
    result2 should be(3)

    val result3 = (list :\ 0)(_ - _) //Short hand
    result3 should be(3)

    val result4 = list.foldRight(0)(_ - _)
    result4 should be(3)

    (5 - (4 - (3 - (2- (1 - 0))))) should be(3)
  }

  test("reduceLeft similar to foldleft but the seed is the head value"){
    val list = List(5, 4, 3, 2, 1)
    list.reduceLeft(_+_) should be(15)

    val strings = List("Do", "Re", "Me", "Fa", "So", "La")
    strings.reduceLeft(_+_) should be("DoReMeFaSoLa")
  }

  test("reduceRight similar to foldRight but the seed is the last value"){
    val list = List(5, 4, 3, 2, 1)
    list.reduceRight(_+_) should be(15)

    val strings = List("Do", "Re", "Me", "Fa", "So", "La")
    strings.reduceRight(_+_) should be("DoReMeFaSoLa")
  }

  test("sum,product,max,min"){
    val list = List(5, 4, 3, 2, 1)
    list.sum should be(15)
    list.product should be(120)
    list.max should be(5)
    list.min should be(1)
  }

  test("reduce is the same than reduceLeft and fold is the same than foldLeft"){
    val intList = List(1,2,3,4,5)
    intList.reduceRight((x,y) => x - y) should be(3)
    intList.reverse.reduceLeft((x, y) => y -x) should be(3)
    intList.reverse.reduce((x,y) => y -x) should be(3)
  }

  test("transpose"){
    val list = List(List(1,2,3), List(4,5,6), List(7,8,9))
    list.transpose should be(List(List(1,4,7), List(2,5,8), List(3,6,9)))
  }

  test("mkString"){
    val list = List(1, 2, 3, 4, 5)
    list.mkString(",") should be("1,2,3,4,5")

    //beginning and ending
    list.mkString(">", ",", "<") should be(">1,2,3,4,5<")
  }

  test("StringBuilder"){
    val stringBuilder = new StringBuilder()
    val list = List(1,2,3,4,5,6,7,8,9)
    stringBuilder.append("I want all numbers 6-12: ")
    list.filter(it => it > 5 && it < 13).addString(stringBuilder, ",")
    println(stringBuilder.mkString)

    val set = Set(1, 9, 10, 22)
    val list2 = List(3, 4, 5, 10)
    val result = set ++ list2
    println(result.size)

    val result2 = list2 ++ set
    println(result2.size)
  }


}
