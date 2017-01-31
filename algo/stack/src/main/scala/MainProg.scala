package main.scala 
import main.scala._
import io.StdIn._

object TestMain {
  def main(args: Array[String]): Unit = {
    var option = 0
    while(option != 99)
    {
       println(menu)
       option = readInt()
       option match {
         case 1 => {println("Starting RPN calc, type q to quit")
                    rpnCalculator
                    }
         case 2 => {println("Starting BFS maze")
                    mazeChecker
                    }
         case _ => "option not recognised"
       }
    }
    println("Goodbye")
  }

  def rpnCalculator:Unit = {
    var option = "" 
    while(option != "q")
    {
      option = readLine()
      val s = option.trim.split(" +").toSeq
      val m = collection.Map[String,Double]()
      val result = RPNCalc(s,m)
      println(result)
    }
  }

  def mazeChecker: Unit = {
   val maze = new Maze 
   val result = maze.breadthFirstShortestPath
   result.get.map(println(_))
  }

  //ditto for queue's program.



  private val menu = """Select one of the following (type 99 to quit)
  1. Use RPN calc
  2. Use BFS Maze"""
}

    
