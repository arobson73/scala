import scala.collection.mutable.Map
object TestApp {
    def wordCount(s:String) = {
      val as = s.split(" ").toList
      val m = as.foldLeft(Map.empty[String,Int]){ (m, word) =>
         m + (word -> (m.getOrElse(word,0) + 1))}
      m
    }
    def main(args: Array[String]): Unit = {
      val wc = wordCount("olly olly in come free in in")
      println(wc)
    }
}
