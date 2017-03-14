
object MathGrammer {

  val White = fastparse.WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import White._
  //note here we could use spire to create a generic math function
  //rather than explicitly using Int / Double. (really we only need double anyway!)
  def eval(tree: (Int, Seq[(String, Int)])) = {
    val (base, ops) = tree
    ops.foldLeft(base) { case (left, (op, right)) => op match {
      case "+" => left + right
      case "-" => left - right
      case "*" => left * right
      case "/" => left / right
    }
    }
  }
  def eval(tree: (Double, Seq[(String, Double)])) = {
    val (base, ops) = tree
    ops.foldLeft(base) { case (left, (op, right)) => op match {
      case "+" => left + right
      case "-" => left - right
      case "*" => left * right
      case "/" => left / right
      case "^" => Math.pow(left, right)
      case "%" => left % right
    }
    }
  }

  /*rep repeats 1 or more times
  * ! captures text parsed by this as String
  * ~/ stops backtracking
  * ? parses this optionlally*/
  val integer: P[Int] = P(CharIn('0' to '9').rep(1).!.map(_.toInt))
  val parens: P[Double] = P("(" ~/ addSub ~ ")")
  val trig: P[Double] = P("sin(" ~/ addSub ~ ")").map(x => Math.sin(x))
  val factor: P[Double] = P(double | parens | trig )
  val divMul: P[Double] = P(factor ~ (CharIn("*/^%").! ~/ factor).rep).map(eval)
  val addSub: P[Double] = P(divMul ~ (CharIn("+-").! ~/ divMul).rep).map(eval)
  val expr: P[Double] = P(" ".rep ~ addSub ~ " ".rep ~ End)
  val digits = P(CharsWhile('0' to '9' contains (_: Char)))
  val exponent = P(CharIn("eE") ~ CharIn("+-").? ~ digits)
  val fractional = P("." ~ digits)
  val integral = P("0" | CharIn('1' to '9') ~ digits.?)
  val double = P(CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map(x => x.toDouble)

}

object TestApp {

  import MathGrammer._
  def main(args: Array[String]): Unit = {
    //try a number
    var r = integer.parse("3")
    println("number is " + r)
    r = integer.parse("57")
    println("number is " + r)
    //try double number
    var t = double.parse("3.1")
    println("number is " + t)
    t = double.parse("32e-12")
    println("number is " + t)
    t = trig.parse("sin(2.1)")
    println("number is " + t)
    t = expr.parse("sin(2.1)*2")
    println("number is " + t)
    t = expr.parse("3+(sin(2.1)*2)")
    println("number is " + t)
    t = expr.parse("sin(3+(sin(2.1)*2))")
    println("number is " + t)
    t = expr.parse("sin( 3+ ( sin( 2.1 ) * 2 ) )")
    println("number is " + t)
  }
}
