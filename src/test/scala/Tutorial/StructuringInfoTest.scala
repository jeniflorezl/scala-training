package Tutorial

import org.scalatest.FunSuite
import org.scalatest.Matchers._


class StructuringInfoTest extends FunSuite{
  test("sealed trait"){
    sealed trait Symbol
    case class Note(name: String, duration: String, octave: Int) extends Symbol
    case class Rest(duration: String) extends Symbol

    val symbol1 = Note("C", "Quarter", 3)
    val symbol2 = Rest("Whole")

    symbol1.name should be("C")
    symbol2.duration should be("Whole")

    //using pattern matching to difference
    def symbolDuration(symbol: Symbol): String =
      symbol match {
          //extracts the parameters depending of type of Symbol
        case Note(name: String, duration: String, octave: Int) => duration
        case Rest(duration: String) => duration
      }

    val symbol3 = Note("A", "Half", 4)
    symbolDuration(symbol3) should be("Half")

    //fixed alternatives, are case object because no aggregate information
    sealed trait NoteName
    case object A extends NoteName
    case object B extends NoteName

    //note durations
    sealed trait Duration
    case object Whole extends Duration
    case object Half extends Duration
    case object Quarter extends Duration

    def fractionWhole(duration: Duration) = duration match {
      case Whole => 1.0
      case Half => 0.5
      case Quarter => 0.25
      }

    val duration1 = Half
    fractionWhole(duration1) should be(0.5)
  }

}
