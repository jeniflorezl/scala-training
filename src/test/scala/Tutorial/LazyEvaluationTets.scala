package Tutorial

import org.scalatest.FunSuite

class LazyEvaluationTets extends FunSuite{
  test("streamRange"){
    var rec = 0
    def streamRange(lo: Int, hi: Int): Stream[Int] = {
      rec = rec + 1
      if (lo >= hi) Stream.empty
      else Stream.cons(lo, streamRange(lo + 1, hi))
    }
    streamRange(1, 10).take(3).toList
    println(rec)
  }

  test("lazy vals and streams"){
    val builder = new StringBuilder
    val x = {builder += 'x'; 1}
    lazy val  y = {builder += 'y'; 2}
    def z = {builder += 'z'; 3}
    z + y + x + z + y + x
    println(builder.result())
  }

}
