package Std

object Solution {
  def main(args: String): Unit ={
    println(io.Source.stdin.getLines().take(2).map(_.toInt).sum)
  }

}
