package oobasics.bank

case class PostCode(postcode: String) 

object PostCodeUtil
{
  def defaultPostcodes: List[String] = List("ABC","EFG","HIJ")
  def getPostCode(lines: List[String]): String = {
    val p =   lines.filter(ln => defaultPostcodes contains ln)
    if (p.size > 0)
      return p(0)
    else
      ""
  }
}

