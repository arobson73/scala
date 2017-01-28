package oobasics.bank
// in practice the address should be entered separately, then
// no need to parse the address into its components
class Address(val lines: List[String]) {
  private var _address: String = "" 
  private var _counties: List[String] = List()
  private var _postcodes: List[String] = List()

  //in real you would create a postcode class and a counties class

  def validateAddress(address: List[String]):String =
  {
    //so many ways to do this, like reading database of address.
    //just check for county and postcodes
    val list_checks = List(getDefaultCounties,getDefaultPostCodes)
    /*
    for(ch <- list_checks;ad <- address)
    {
      if(ch contains ad)

    }*/
   "ff"

  }
  def getDefaultCounties:List[String] =
  {
    //could read this from a file
    val def_counties = List[String]("CLEVELAND,SURREY")
    def_counties
  }
  def getDefaultPostCodes:List[String] =
  {
    val def_postcodes = List[String]("ABC,DEF,GHI")
    def_postcodes
  }


}
