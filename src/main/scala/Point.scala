case class Point(x: Int, y: Int){
  override def toString(): String = "(" +x + "," + y + ")"
}

object Point{
  def main(args: Array[String]): Unit ={
    val pt = new Point(1,2)
    println(pt)
  }
}

class ClassWithValParameter(val name: String)



