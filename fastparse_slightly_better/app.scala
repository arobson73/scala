package Calc
import fastparse.WhitespaceApi
import fastparse.all._
import java.util.Scanner
import scala.io.Source

import scala.util.{Failure, Success, Try}
import math.{cos, sin,pow}
object TestApp {

  val White = WhitespaceApi.Wrapper{
    NoTrace(" ".rep)
  }

  import fastparse.noApi._
  import White._
  def eval(tree: (Double, Seq[(String, Double)])) = {
    val (base, ops) = tree
    ops.foldLeft(base){ case (left, (op, right)) => op match{
      case "+" => left + right case "-" => left - right
      case "*" => left * right case "/" => left / right /*case "^" => pow(left,right)*/
    }}
  }
  val factor: P[Double] = P( number | parens | trig | pwr)
  val divMul: P[Double] = P( factor ~ (CharIn("*/").! ~/ factor).rep ).map(eval)
  val addSub: P[Double] = P( divMul ~ (CharIn("+-").! ~/ divMul).rep ).map(eval)
  val expr: P[Double]   = P(" ".rep ~ addSub ~ " ".rep ~ End )
  val parens: P[Double] = P( "(" ~/ addSub ~ ")" )
  val digits = P( CharsWhileIn("0123456789"))
  val fractional = P( "." ~ digits )
  val integral = P( "0" | CharIn('1' to '9') ~ digits.? )
  val exponent = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
  val number = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(_.toDouble)
  val sinp = P("sin" ~/ parens).map(sin(_))
  val cosp = P("cos" ~/ parens).map(cos(_))
  val trig = P(sinp | cosp)
  val pwr = P("pow(" ~/ number.! ~/ "," ~/ number.! ~ ")" ).map({case (x,y)  => pow(x.toDouble,y.toDouble)})


  def main(args: Array[String]): Unit = {
    val scanner = new Scanner(System.in)
    println("Enter calcs:")
    while (true)
      {
        val input = scanner.nextLine()
        if (input != null) {
          println(s"Calc for :  $input")
          val res = expr.parse(input)
          println(s"result is $res")
        }
      }
  }

  def testFuncs = {
    val Parsed.Success(2, _) = expr.parse("1+1")
    val Parsed.Success(15, _) = expr.parse("(1+1*2)+3*4")
    val Parsed.Success(21, _) = expr.parse("((1+1*2)+(3*4*5))/3")
    val Parsed.Failure(expected, failIndex, extra) = expr.parse("1+1*")
    assert(expected == (number | parens | trig | pwr), failIndex == 4)

    //try running some numbers
    val r1 = expr.parse("(4.8+8.6)*2.2")
    val res = r1 match {
      case Parsed.Success(a,_) => a
      case Parsed.Failure(f,_,_) => f
    }
    println(res)
    println(sinp.parse("sin(0.5)"))
    println(expr.parse("2 * cos(0.25) + 3"))
    println(expr.parse("2 * 2 * 2e3"))
    println(expr.parse("6+7*10"))
    println(expr.parse("2e3"))
    println(expr.parse("1e3"))
    println("parse power")
    println(pwr.parse("pow(3,4)"))
    println(expr.parse("(2 * 3 + 1)*sin(0.8) / pow(2,3)"))
    println(expr.parse(" pow(2.1,3.4)"))

  }
}
