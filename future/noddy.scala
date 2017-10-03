package com.andyr
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
object TestApp {
    case class Simple(c:Int)
    def doubleIt(x:Simple):Future[Int] = x match {
        case Simple(x) => Future{x*2}
    }
    //here we can take a Simple type, pass the Simple type to doubleIt, which changes the type to Future[Int]
    //we can then use a map to to take int and create a Simple type again. Pointless but shows how types
    //can be changed
    def process(in:List[Int]):Future[Simple] = {
        in.foldLeft(Future(Simple(1)))(
            (p,_) => p.flatMap(x => doubleIt(x)).map{s =>
                Simple(s)}
        )
    }
    //this just uses the init value (1) and then *2 for the length of the list (so like power 2^(len of list))
    def process2(in:List[Int]):Future[Int] = {
        in.foldLeft(Future{1})(
            (p,_) => p.flatMap(x => Future(x*2))
        )
    }
    //rather pointless accumulator is ignored, and current value from list is doubled. Final result is last value
    //in the list * 2
    def process3(in:List[Int]):Future[Int] = {
        in.foldLeft(Future{1})(
            (p,v) => p.flatMap(_ => Future(v*2))
        )
    }
    //uses the accumulator now
    def process4(in:List[Int]):Future[Int] = {
        in.foldLeft(Future{1})(
            (p,v) => p.flatMap(x => Future(x + v*2))
        )
    }
    //each val from accumualtor is *2 then / 2, so final result is 1
    def process5(in:List[Int]):Future[Int] = {
        in.foldLeft(Future{1})(
            (p,_) => p.flatMap(x => Future(x*2)).map(n => n/2)
        )
    }
    def mainProc[A](in:List[A],fl:List[List[A] => Future[A]]): Unit = {
        fl.map(x => {
            val f = x(in)
            f onComplete {
                case Success(x) => println(s"$x")
                case Failure(e) => println(s"$e")
            }
            Await.result(f,2 seconds)
        }
        )
    }

    def main(args: Array[String]): Unit = {
        val lst = (1 to 5).toList
        val lf = List(process2(_),process3(_), process4(_),process5(_))
        mainProc(lst,lf)

        val f = doubleIt(Simple(3))
        f onComplete {
            case Success(x) => println(s"$x")
            case Failure(e) => println(s"$e")
        }
        Await.result(f,2 seconds)

    }
}
