import scala.io.Source
import scala.annotation.tailrec
object FS {
  //not used in this program so far
  def removeStrs(line: String, filt: List[String]): String = {
    @tailrec
    def helper(acc:Array[String], r: List[String]):Array[String] = {
      if (r.isEmpty) acc
      else helper(acc.filterNot(_ == r.head),r.tail)
    }
    val a = line.split(" ")
    helper(a,filt).mkString(" ")

  }
  def removeBetweenExcept(line: String, marker1:String, marker2:String, except: String): String = {
    //println(line)
    val sl = line.split(" ").filterNot(_.isEmpty) // split line on spaces, remove empty spaces
    //println(sl.mkString((" ")))
    //sl.foreach(x => println(marker1,x.head.toString,x.last.toString,x))
    val slf = sl.filterNot(x => x.head.toString == marker1 && x.last.toString == marker2 && !x.contains(except))
    slf.mkString(" ")

  }

  def removeDateKeepTime(line: String, year:String): String ={
    val sl = line.split(" ").filterNot(_.isEmpty)
    val slf = sl.filterNot(_.contains(year))
    slf.mkString(" ")
  }

  /*keep the line if it contains any of the filters*/
  def keepLine(line: String, filt: List[String]):Boolean = {
    @tailrec
    def helper(kr:List[String]): Boolean = {
      if (kr.isEmpty) false
      else {
        val r = line contains kr.head
        if (r == true) true
        else helper(kr.tail)
      }
    }
    helper(filt)
  }

  def commandLineValid(option:String, fils:List[String]):Boolean = {
    val r = option match {
      case "trimer" => if (fils.size == 3) true else {
        println("need 3 arguments for trimer program")
        false
      }
      case "removedatekeeptime" => if(fils.size == 1) true else {
        println("need 1 argument for removedatekeeptime program")
        false
      }
      case _ => true
    }
    r
  }

  def main(args: Array[String]): Unit = {

    if(args.length  <= 2) {
      println("Need to supply input file, option remove or keep and strings to remove from file")
      println("Like this: scala filter_file.scala infile.txt keep somestring1 somestring2 somestringx")
      return
    }
    val arguments = args.toList
    val name = arguments.head
    val option = arguments.drop(1).head
    val fils = arguments.drop(1).tail
    val filsvec =fils.toVector

    if (!commandLineValid(option,fils)) return

    println("Filename is " + name)
    println("option is " + option)
    println("filters are " + fils)
    val source  = Source.fromFile(arguments.head)("UTF-8")
    //note this could be improved so we make selection (whether its keep or remove) before the looping
    //then use the loop inside the function, passing the source into the function. This is just a
    //script
    for (line <- source.getLines())
    {
        option match {
          case "keep" if (keepLine(line, fils)) => println(line)
          case "remove" if (!keepLine(line, fils)) => println(line)
          case "trimer" => println(removeBetweenExcept(line, filsvec(0), filsvec(1), filsvec(2)))
          case "removedatekeeptime" => println(removeDateKeepTime(line,filsvec(0)))
          case _ => {
            println("unknown option")
          }
        }
    }
    source.close
  }
}
