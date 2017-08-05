import scala.io.Source
//sbt "runMain WordCount file"
object WordCount {
    def main(args: Array[String]): Unit = {
        val r = args.map(file =>
        Source.fromFile(file)("utf8").
        getLines.flatMap(_.split("\\W+")).
        foldLeft(Map.empty[String,Int]){
            (m,word) =>
          m + (word -> (m.getOrElse(word,0) + 1 ))
        }
        )
        for ((elem,i) <- r.zipWithIndex){
            println(">>file " + i+1)
            elem.foreach(v => println(v._1,v._2))}
}

}
