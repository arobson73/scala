  import scala.concurrent._
import akka._
import akka.actor._
import akka.stream._
import akka.stream.scaladsl._
import akka.util._
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global


object TestApp {
    def main(args: Array[String]): Unit = {
        implicit val system = ActorSystem("TestSystem")
        implicit val materializer = ActorMaterializer()
        val listOfFruits = Source(List("Pears","Apples","Oranges"))
        val result :Future[(String,Boolean)] = listOfFruits.map(fruit => (fruit,findFruit(fruit,"Apples"))).runWith(Sink.head)
        result.onComplete(println(_))

        val result2 :Future[Boolean] = listOfFruits.map(fruit => findFruit(fruit,"Apples")).runWith(Sink.head)
        result2.onComplete(println(_))

        val result1: Future[Boolean] = listOfFruits.collect({case x:String if x== "Apples" => true; case _ => false}).runWith(Sink.head)
        result1.onComplete((println(_)))

        val result4: Future[Boolean] = listOfFruits.runWith(Sink.head).map(_ == "Apples")
        result4.onComplete(println(_))

    }
    def findFruit(fruit:String, ref:String):Boolean =
    {
      return fruit == ref
    }

    val  m:PartialFunction[String,Boolean] = {
        case x:String if x == "Apples" => true
        case _ => false
    }

}
