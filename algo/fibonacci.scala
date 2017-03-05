import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer}
object Fib {
  @tailrec
  def fib(n:Int, f1: Long, f0: Long): Long = {
  if (n > 0) fib(n-1, f1+f0,f1) else f1+f0
  }
  def fibseq(n:Int, f1:Long,f0: Long):Vector[Long] ={
    var arr = ArrayBuffer[Long](f0,f1)
    @tailrec
    def helper(i:Int,a1:Long, a0:Long):Unit = {
      if (i > 0)
        {
          val nxt = a1+a0
          arr.append(nxt)
          helper(i-1,nxt,a1)
        }
    }
    helper(n,f1,f0)
    arr.toVector
    }
  def main(args: Array[String]): Unit =
  {
    val r = fib(10,1,0)
    println("result is " + r)
    val s = fibseq(10,1,0)
    s.foreach(v => print(v + " " ))
  }
}
