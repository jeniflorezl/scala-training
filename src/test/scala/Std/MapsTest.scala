package Std

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class MapsTest extends FunSuite{

  test("creating a map"){
    val myMap = Map("MI" -> "Michigan", "OH" -> "Ohio",
      "WI"-> "Wisconsin", "MI" -> "Michigan")
    myMap.size should be(3)

    //Add maps
    val aNewMap = myMap + ("IL"->"Ill inois")

    aNewMap.size should be(4)

  }

  test("Iterating maps"){
    val myMap = Map("MI" -> "Michigan", "OH" -> "Ohio",
      "WI"-> "Wisconsin", "MI" -> "Michigan")
    val myMapValues = myMap.values

    myMapValues.head should be("Michigan")
    myMapValues.last should be("Wisconsin")
  }

  test("Accessing to maps"){
    val myMap = Map("MI" -> "Michigan", "OH" -> "Ohio",
      "WI"-> "Wisconsin", "MI" -> "Michigan")

    myMap("MI") should be("Michigan")
  }

  test("Changes duplicate keys"){
    val myMap = Map("MI" -> "Michigan", "OH" -> "Ohio",
      "WI"-> "Wisconsin", "MI" -> "Meechigan")

    myMap("MI") should be("Meechigan")
  }

  test("mixed type"){
    val myMap = Map("Ann Arbor" -> "MI", 49931 -> "MI")
    myMap("Ann Arbor") should be("MI")
    myMap(49931) should be("MI")
  }

  test("missing key"){
    val myMap = Map("MI" -> "Michigan", "OH" -> "Ohio",
      "WI"-> "Wisconsin", "MI" -> "Michigan")
    intercept[NoSuchElementException]{
      myMap("TY")
    }
    myMap.getOrElse("TX", "missing data") should be("missing data")

    val myMap2 = Map("MI" -> "Michigan", "OH"->"Ohio", "WI"->"Wisconsin",
    "IA"->"Iowa") withDefaultValue "missing data"

    myMap2("TI") should be("missing data")
  }
  
  test("removing of maps"){
    val myMap = Map("Jeniffer" -> 22, "Angela"-> 23, "Camilo"->21)
    val newMap = myMap - "Jeniffer"

    newMap.contains("Jeniffer") should be(false)

    //removing with lists

    val newMap2 = myMap -- List("Jeniffer", "Angela")
    newMap2 should be(Map("Camilo"->21))

    val newMap3 = myMap - ("Jeniffer","Angela")
    newMap3 should be(Map("Camilo"->21))
  }

  test("removing no exist key from map"){
    val myMap =
      Map("MI" -> "Michigan", "OH" -> "Ohio", "WI" -> "Wisconsin", "IA" -> "Iowa")
    val aNewMap = myMap - "MN"

    aNewMap.equals(myMap) should be(true)
  }

  test("comparing two maps with different order"){
    val myMap1 =
      Map("MI" → "Michigan", "OH" → "Ohio", "WI" → "Wisconsin", "IA" → "Iowa")
    val myMap2 =
      Map("WI" → "Wisconsin", "MI" → "Michigan", "IA" → "Iowa", "OH" → "Ohio")

    myMap1.equals(myMap2) should be(true)
  }

  test("new test map"){
    val mapa = Map("1" -> 1, "2" -> 2, "4" -> 4)
    val res = for {
      valor <- mapa
      if valor._2 % 2 == 0
    }yield valor
    res should be(Map("2" -> 2, "4" -> 4))
    assert(res.values.size == 2)
    assert(res.values.headOption == Some(2))
  }

  test("adding to map"){
    val map = Map(1 -> "Ricardo", 2 -> "Santiago", 3 -> "Sara")
    assertResult(Map(1 -> "Ricardo", 2 -> "Santiago", 3 -> "Sara", 4 -> "Andrea")){
      map + (4 -> "Andrea")
    }
  }

  test("headOption map"){
    val map = Map("Antioquia" -> "Medellin", "Cundinamarca" -> "Bogota")
    assertResult(Some(("Antioquia" -> "Medellin"))){
      map.headOption
    }
  }

  test("verification with tupla"){
    val map = Map("Antioquia" -> "Medellin", "Cundinamarca" -> "Bogota")
    //cada elemento se comporta como una tupla
    assertResult(("Antioquia")){
      val headO = map.headOption
      headO.get._1
    }
  }

  test("splitAt to map"){
    val map = Map("Antioquia" -> "Medellin", "Cundinamarca" -> "Bogota", "Risaralda" -> "Pereira")
    val (map2, map3) = map.splitAt(2)
    assert(map2 == Map("Antioquia" -> "Medellin", "Cundinamarca" -> "Bogota"))
     assert(map3 == Map("Risaralda" -> "Pereira"))
  }

  test("filter en un map"){
    val map = Map(1 -> "Hola", 2 -> "Hi", 3 -> "Bye", 4 -> "GoodBye")
    val res = map.filter(m => m._2.startsWith("H"))
    assert(res == Map(1 -> "Hola", 2 -> "Hi"))
  }

  test("map with Maps"){
    val map = Map(1 -> "Hola", 2 -> "Hi", 3 -> "Bye", 4 -> "GoodBye")
    val res = map.map(m => (m._1, m._2 + " !"))

    assert(res == Map(1 -> "Hola !", 2 -> "Hi !", 3 -> "Bye !", 4 -> "GoodBye !"))

    //with mapValues
    val res2 = map.mapValues(m => m + " !")
    assert(res2 == Map(1 -> "Hola !", 2 -> "Hi !", 3 -> "Bye !", 4 -> "GoodBye !"))
  }
}
