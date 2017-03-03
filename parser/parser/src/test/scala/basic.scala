import collection.mutable.Stack
import org.scalatest._
import Reference._
import Parsers._
class StackSpec extends FlatSpec {

  "many1(char(a))"should "return Left(1.1 a) for input of just \"b\"" in {
    val m = many1(char('a'))
    val r = Reference.run(m)("b")
    val s = r.toString
    assert(s.contains("1.1"))
    assert(s.contains("a"))
    assert(s.contains("Left"))
  }
  "many(char(a))" should "return Right(List()) for input of just \"b\"" in {
    val m = many(char('a'))
    val r = Reference.run(m)("b")
    val s = r.toString
    assert(s == "Right(List())")
  }
   "many(char(a))" should "return Right(List(a)) for input of just \"a\"" in {
    val m = many(char('a'))
    val r = Reference.run(m)("a")
    val s = r.toString
    assert(s == "Right(List(a))")
  }
  "many1(char(a))" should "return Right(List(a)) for input of just \"a\"" in {
    val m = many1(char('a'))
    val r = Reference.run(m)("a")
    val s = r.toString
    assert(s == "Right(List(a))")
  }
  "many1(char(a))" should "return Left(1.1 'a') for input of just \"\" " in {
    val m = many1(char('a'))
    val r = Reference.run(m)("")
    val s = r.toString
    assert(s.contains("1.1"))
    assert(s.contains("a"))
    assert(s.contains("Left"))
  }
    "many(char(a))" should "return Right(List()) for input of just \"\"" in {
    val m = many(char('a'))
    val r = Reference.run(m)("")
    val s = r.toString
    assert(s == "Right(List())")
  }
   "many(char(a))" should "return Right(List(a, a, a)) for input of just \"aaa\"" in {
    val m = many(char('a'))
    val r = Reference.run(m)("aaa")
    val s = r.toString
    assert(s == "Right(List(a, a, a))")
  }
    "many(char(a))" should "return Right(List(a, a)) for input of just \"aab\"" in {
    val m = many(char('a'))
    val r = Reference.run(m)("aab")
    val s = r.toString
    assert(s == "Right(List(a, a))")
  }
    "many1(char(a))" should "return Right(List(a, a)) for input of just \"aab\"" in {
    val m = many1(char('a'))
    val r = Reference.run(m)("aab")
    val s = r.toString
    assert(s == "Right(List(a, a))")
  }
}