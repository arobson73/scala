import Reference._
import Parsers._

object TestApp {
  def main(args: Array[String]): Unit = {
    println("hello")
    val a  = char('a')
    val b = many(a)
    val c = Reference.run(b)("a")
    //println(b)
  }
}

