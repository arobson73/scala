import scala.annotation.tailrec

object Arith99 {
  def primeSeq(mx: Int): Array[Boolean]  = {
    val flags = Array.fill(mx+1)(true)
    println(flags.size)
    val prime = 2
    @inline def setFlag(i:Int):Unit = {
      try {
        flags(i) = false
      }
      catch{
        case x: ArrayIndexOutOfBoundsException => {
          println(s"OOB " + i)
          throw new ArrayIndexOutOfBoundsException
        }
      }
    }
    @inline def readFlag(i:Int):Boolean ={
      !flags(i)
    }
    def crossOff(p:Int):Unit = {
      @tailrec
      def loop(i:Int): Unit = {
        if (i < (mx + 1)) {
          setFlag(i)
          loop(i+p)
        }
      }
      val in = p*p
      println(s"in =$in")
      loop(p*p)
    }
    def getNextPrime(p:Int):Int = {
      @tailrec
      def loop(i:Int) : Int = {
        if(i < (mx + 1) && readFlag(i)) loop(i+1)
        else i
      }
      loop(p+1)
    }
    @tailrec
    def loopOuter(p:Int) : Unit = {
      crossOff(p)
      val nxp = getNextPrime(p)
      println(" "+ nxp)
      if(nxp < (mx+1)) loopOuter(nxp)
    }
    //called code
    loopOuter(prime)
    flags
  }
}
object SoEwithArray {
  def makeSoE_PrimesTo(top: Int) = {
    import scala.annotation.tailrec
    val topNdx = (top - 3) / 2 + 1 //odds composite BitSet buffer offset down to 3 plus 1 for overflow
    //divide array size by 32 sine we hare just using the bits. use sqrt since if have a number n that is
    //not prime, then n = a*b . hence it has factors a and b. if we use sqrt then we only care about
    //factors less than or equal to sqrt. for example n=100, if 10*10 or 5 * 20, or 2 * 50 or 4 * 25.
    //we can tell if its not prime by looking at factors that are 10 or lower.
    //Note also do -3 /2 just to get all odd numbers, since we know an even number will never be prime.
    val (cmpsts, sqrtLmtNdx) = (new Array[Int]((topNdx >>> 5) + 1), (Math.sqrt(top).toInt - 3) / 2)
    //composite number of != 0, prime if == 0
    @inline def isCmpst(ci: Int): Boolean = (cmpsts(ci >>> 5) & (1 << (ci & 31))) != 0

    @inline def setCmpst(ci: Int): Unit = cmpsts(ci >>> 5) |= 1 << (ci & 31)
    //running with input of top = 20
    // by default everything is prime initially that is all zeros in the Array
    //we first start with 0. convert this to its prime, which is 3. then call
    //cullPrm passing in 9 - 3 = 6 /2 = 3 here we just square it and keep to the
    // odd number rule. hence 9 is not prime so we mark the array with 1 at 3rd bit location
    //bit 0 is first position which corresponds to 3. we then increment to next non prime bit
    //which will be 3 + 3 = 6th bit. the 6th bit (2*6 + 3) is 15 which is not prime. again we icrement
    //bit pos by 3 to the 9th bit position (9*2 + 3 = 21) which is not prime.
    //
    @tailrec
    def forCndNdxsFrom(cndNdx: Int): Unit = {
      if (cndNdx <= sqrtLmtNdx) {
        cmpsts.foreach(v => println(s"inA:${v.toBinaryString}"))
        if (!isCmpst(cndNdx)) {
          //is prime
          val p = cndNdx + cndNdx + 3
          println(s"in=$cndNdx,p=$p,topNdx=$topNdx")

          @tailrec def cullPrmCmpstsFrom(cmpstNdx: Int): Unit =
            if (cmpstNdx <= topNdx) {
              setCmpst(cmpstNdx);
              //  println(s"cull $cmpstNdx\n ")
              cmpsts.foreach(v => println(s"A:${v.toBinaryString}"))
              cullPrmCmpstsFrom(cmpstNdx + p)
            }

          cullPrmCmpstsFrom((p * p - 3) >>> 1)
        }
        forCndNdxsFrom(cndNdx + 1) //this repeats this time from 1. bit 1 is not composite
        //this time p = 1+1+3 = 5. hence 5*5 = 25 - 3 = 22 / 2 = 11. in our use case of top = 20,
        // then topNdx is 20-3 / 2 + 1 = 9, so we do not go around again.
      }
    }
    forCndNdxsFrom(0)

    @tailrec def getNxtPrmFrom(cndNdx: Int): Int =
      if ((cndNdx > topNdx) || !isCmpst(cndNdx)) {
        val n = cndNdx + cndNdx + 3 // double it and add 3, bit 1 = 5, bit 2 = 7
        //  println(s"b=$cndNdx,p=$n")
        n
      }
      else
      {
        //println(s"pc=$cndNdx")
        getNxtPrmFrom(cndNdx + 1)
      }
    //this returns the values , and will run forever, hence we use takeWhile at the end
    Iterator.single(2) ++ Iterator.iterate(3)(p => getNxtPrmFrom(((p + 2) - 3) >>> 1)).takeWhile(_ <= top)

  }
}

object TestArith {
  import Arith99._
  import SoEwithArray._
  def main(args:Array[String]):Unit = {
    val top_num = 100000000
    val strt = System.nanoTime()
    //val data = makeSoE_PrimesTo(top_num)
    var g=40
    if (args.size == 1) g = args(0).toInt
    val data = makeSoE_PrimesTo(g)
    //note the iterator is lazy, we only cause it to yield its values in data when we invokde
    // the println
    data.foreach(x => println(x))
    //val count = makeSoE_PrimesTo(top_num).size
    /*
    val ps = primeSeq(100)
    for((b,i) <- ps.zipWithIndex
        if(b == true)) yield { println(i)}
        */
    //val count = primeSeq(top_num).size
    val end   = System.nanoTime()
    println(s"Successfully completed without errors. [total ${(end - strt) / 1000000} ms]")
    //println(f"Found $count primes up to $top_num" + ".")
    println("Using one large mutable Array and tail recursive loops.")
  }
}
