package oobasics.bank
import io.StdIn._

class PhoneNumber(val value: String) {
  // TODO - Code that will verify this is a valid phone number.
  // check the number of digits 07460 209 625
  private var _number: String = validatePhoneNumber(value)   

  def validatePhoneNumber(num: String):String =
  {
    var res = num
    println("Number in is " + res)
    if(res.length != 11)
    {
      println("number must be 11 digits")
      do 
      {
        println("Enter number again, or q to quit!")
        res = readLine()
        println("you entered ")
        println(res)
      }while(res.length != 11 || res == "q")
    }
    res 
  }
  def getNumber:String = _number
  def setMainNumber {
    println("Enter the new number")
    var res = readLine()
    _number = validatePhoneNumber(res)
  }

}
