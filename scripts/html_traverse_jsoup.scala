import org.jsoup.Jsoup
import org.jsoup.nodes.Node
import org.jsoup.select.NodeVisitor
//note there is a better way below to implement the java interface in scala
//in general a complicated api like jsoup, best to look at jsoup source code
//to fully understand
class MyVisitor(var childList:List[String]) extends NodeVisitor {
  override def head(node:Node, depth:Int): Unit = {
    if (node.childNodeSize() == 0) {
      childList = node.toString +: childList
    }
    println("Entering tag : ",node.nodeName())
  }
  override def tail(node: Node, depth: Int): Unit = {
    println("Exiting tag : ",node.nodeName())
  }
}

object TestApp {
  val nodeTravTestString =   "<div id=\"d1\">" + //
    "<div id=\"d1.0\">" + //
    "<div id=\"d1.0.0\">" + //
    "1.0.0" + //
    "</div>" + //
    "<div id=\"d1.0.1\">" + //
    "1.0.1" + //
    "</div>" + //
    "</div>" + //
    "<div id=\"d1.1\">" + //
    "1.1" + //
    "</div>" + //
    "<div id=\"d1.3\">" + //
    "1.3" + //
    "</div>" + //
    "</div>"
  val nodeTravTestString2 = "<html>" +
    "<head>" +
    "<title>Page Title</title>" +
    "</head>" +
    "<body bgcolor=\"ffffff\">" +
    "<a href=\"http://www.google.com\">Google</a>" +
    "<h1>Heading 1</h1>" +
    "<div>div tag <strong>content</strong></div>" +
    "</body>" +
    "</html>"

  def myTraverse(htmlString: String): Unit = {
    val doc = Jsoup.parse(htmlString)
    doc.traverse(new NodeVisitor() {
      def head(node:Node, depth: Int): Unit = {
        println("Entering tag: ", node.nodeName())
      }
      def tail(node:Node, depth:Int): Unit = {
        println("Exiting tag: ",node.nodeName())
      }
    })
  }

  def main(args: Array[String]): Unit = {
    //val file = Source.fromFile("gog.html")("UTF-8")
    //val str = file.mkString
    myTraverse(nodeTravTestString2)

  }
}
