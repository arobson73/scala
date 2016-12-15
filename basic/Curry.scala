object Curry {
  //curry(adds)(3)(4)
  //so curry takes a function that goes from (A,B) => C i.e like and adds function for example. The output is also a function
  //that takes an A => (B => C)
  //val a = curry(adds) gives that output
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a,b)
  }
  def adds(a: Int, b: Int): Int = a + b
  //uncurry(curry(adds))(3,4)
  //uncurry takes an input function (a curried function)
  //its output is a function that takes (a,b) => c
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    (a,b) => f(a)(b)
  }

  def times2(a: Int): Int = a * 2
  def times3(a: Int): Int = a * 3
  //this is a function thats takes in input a and a function that takes a to b. output is b
  def f1[A,B](a: A, f: A => B ): B = f(a)    

  def f2(x:Double): Double = math.Pi/2  - x

  val z = (x: Double) => math.Pi / 2 -x

  //a function that takes 2 input functions, each input function takes one input and produces and output
  //compose(times2,times3)(5)
  def compose[A,B,C](f: B => C, g: A => B) : A => C = {
    (a: A) => f(g(a))
  }
}
