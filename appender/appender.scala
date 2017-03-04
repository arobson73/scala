case class Appender[A](holder: Vector[Vector[A]])
{
  //appends the data to the holder. only append to vectors of stated size (add2Size)
  def addTo(data:Vector[A], add2size: Int):Appender[A] =
  {
    val f = holder.filter(h  => h.size == add2size).map(a => a ++ data)
    Appender(holder ++ f)
  }
  //appends to the holder but in increasing steps from 1 to length of data
  def add2ToR(indata:Vector[A]): Appender[A] =
  {
    def go(app:Appender[A],addSize: Int, data:Vector[A]):Appender[A] = {
      if (!data.isEmpty) {
        val na = app.addTo(Vector(data.head), addSize)
        go(na, addSize+1,data.tail)
      }
      else app
    }
    go(this,1,indata)
  }
  //show the holder
  def show = holder.foreach(v => println(v))
}

object Test {
  def main(args: Array[String]) : Unit = {
    val a = Appender(Vector(Vector(1),Vector(2)))
    val b = a.addTo(Vector(3),1)
    println("Now we have!")
    b.show
    // now add to vectors of size  2
    val c = b.addTo(Vector(4),2)
    println("Now we have!")
    c.show
    println("try the addToR recursion")
    val a2 = Appender(Vector(Vector(1),Vector(2)))
    val b2 = a2.add2ToR(Vector(4,3,5))
    println("Now we have!")
    b2.show
  }
}