package oobasics.bank

case class Counties(county: String)


/*note there is code dupe here with Postcode, room to make 
 * this more generic*/
object CountiesUtil
{
  def defaultCounties: List[String] = List("surrey","kent","yorkshire","hampshire")
  def getCounty(lines: List[String]): String = {
    val p = lines.filter(ln => defaultCounties contains ln)
    if(p.size > 0)
      return(p(0))
    else
      ""
  }
}
