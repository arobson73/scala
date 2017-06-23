import scala.io.Source
import scala.annotation.tailrec
object FS {
  //not used in this program so far
  def removeStrs(str: String, fil: List[String]): String = {
    @tailrec
    def helper(acc:Array[String], r: List[String]):Array[String] = {
      if (r.isEmpty) acc
      else helper(acc.filterNot(_ == r.head),r.tail)
    }
    val a = str.split(" ")
    helper(a,fil).mkString(" ")
    
  }

  /*keep the line if it contains any of the filters*/
  def keepLine(str: String, k: List[String]):Boolean = {
    @tailrec
    def helper(kr:List[String]): Boolean = {
      if (kr.isEmpty) false
      else {
        val r = str contains kr.head
        if (r == true) true
        else helper(kr.tail)
      }
    }
    helper(k)
  }



  def main(args: Array[String]): Unit = {
      
    if(args.length  <= 2) {
      println("Need to supply input file, option remove or keep and strings to remove from file")
      println("Like this: scala filter_file.scala infile.txt keep somestring1 somestring2 somestringx")
      
    }
    else{
      val arguments = args.toList
      val name = arguments.head
      val option = arguments.drop(1).head
      val fils = arguments.drop(1).tail
      println("Filename is " + name)
      println("option is " + option)
      println("filters are " + fils)
      val source  = Source.fromFile(arguments.head)("UTF-8")
      //note this could be improved so we make selection (whether its keep or remove) before the looping
      //then use the loop inside the function, passing the source into the function. This is just a 
      //script
      for (line <- source.getLines())
      {
        
          if (option == "keep")
          {
            if(keepLine(line,fils)) println(line)
          }
          else //remove
          {
            if(!keepLine(line,fils)) println(line)
          }
          
      }
      source.close
      
    }


  }
}
