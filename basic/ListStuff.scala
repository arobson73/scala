//package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`

case object Nil extends List[Nothing] // A `List` data constructor representing the empty list

/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,

which may be `Nil` or another `Cons`.

*/
//not here Cons does not get the "tail", this is just a name of variable!
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  }

  def head[A](l: List[A]): A = l match {
    case Nil => sys.error("Empty List") 
    case Cons(a,t) => a
  }

  def tail[A](data: List[A]): List[A] = data match {
    case Nil => Nil
    case Cons(_,xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil 
    case Cons(_,t) => Cons(h,t)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def drop[A](l:List[A], n: Int): List[A] = 
    if ( n <= 0)
      l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t,n-1)
    }
    //dropWhile(List(1,2,3,4,5,6,7,8), (x : Int) => x < 4)
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(a,t) if f(a) => dropWhile(t,f)
    case _ => l
  }

  def append[A](a: List[A], b: List[A]): List[A] = a match {
    case Nil => b
    case Cons(h,t) => Cons(h, append(t,b))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_,Nil) => Nil
    case Cons(h,xs) => Cons(h,init(xs))
  }
  //same as drop while but no need to specify type. now call like this
  //dropWhile2(List(1,2,3,4,5,6,7,8))(x => x < 4)
  def dropWhile2[A](l: List[A])(f: A=> Boolean): List[A] = l match {
    case Cons(a,t) if f(a) => dropWhile2(t)(f)
    case _ => l
  }
  //not foldRight is not stack safe for large lists!
  //foldLeft is better
  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B) : B = as match {
    case Nil => z
    case Cons(x,xs) => f(x,foldRight(xs,z)(f))
  }

  def length[A](as: List[A]): Int = foldRight(as,0)((_,acc) => acc+1)  

  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = as match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs,f(z,x))(f)
  }
  
  def sumf(ints: List[Int]): Int = foldLeft(ints,0)((x,y) => x + y)
  def productf(l: List[Double]): Double = foldLeft(l,1.0)((x,y) => x*y)
  def length2[A](as: List[A]): Int = foldLeft(as,0)((acc,_) => acc+1)
  
  def reverse[A](as: List[A]): List[A] = foldLeft(as,List[A]())((x,y) => Cons(y,x))

}
