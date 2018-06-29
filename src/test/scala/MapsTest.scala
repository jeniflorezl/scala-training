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
    val myMap = Map("Ann Arbor" → "MI", 49931 → "MI")
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
      Map("MI" → "Michigan", "OH" → "Ohio", "WI" → "Wisconsin", "IA" → "Iowa")
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
}
