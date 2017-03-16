object Test {
  //note this is just for simple quadratic like ax^2+bx+c
  def getCoefficients(s:String) = {
    val sc = s.filterNot((x:Char) => x.isWhitespace)
    sc.split('x').map(v => v.replace("^2","")).map(_.toDouble)
  }
  def main(args: Array[String]) : Unit = {

    val in = "-174.5x^2 + 34.6x + 55.7"
    val r = getCoefficients(in)
    r.foreach(println(_))
  }
}
