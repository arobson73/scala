//import Stream._
//
:paste
trait Stream[+A] {
  //strm.toList
 def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }
 //val x = Stream.cons(1,Stream.cons(2,Stream.cons(3,Stream.empty)))
 //x.take(3).toList
 def take(n: Int): Stream[A] = this match {
   case Cons(h,t) if (n > 1) => Stream.cons(h(),t().take(n - 1))
   case Cons(h,_) if (n == 1) => Stream.cons(h(),Stream.empty)
   case _ => Stream.empty
 }

 def takeViaUnfold(n: Int): Stream[A] = 
   Stream.unfold((this,n)) {
     case (Cons(h,t),1) => Some(h(),(Stream.empty,0))
     case (Cons(h,t),n) if n > 1 => Some(h(),(t(),n-1))
     case _ => None
   }
 //x.drop(4).toList
 def drop(n: Int): Stream[A] = this match {
   case Cons(h,t) if (n > 0) => t().drop(n-1)
   case _ => this 
 }
  //x.takeWhile(x => x < 4).toList
 def takeWhile(p: A => Boolean): Stream[A] = this match
 {
   case Cons(h,t) if p(h()) => Stream.cons(h(),t().takeWhile(p))
   case _ => Stream.empty
 }

 def takeWhileWithUnfold(p: A => Boolean): Stream[A] =
   Stream.unfold(this) {
     case Cons(h,t) if p(h()) => Some(h(),t())
     case _ => None
   }
 def exists(p: A => Boolean): Boolean = this match {
   case Cons(h,t) => p(h()) || t().exists(p)
   case _ => false
 }
 //Stream(1,2,3,4,5).foldRight(1)((x,y) => x*y) 
 def foldRight[B](z: => B)(f: (A, => B) => B): B = 
   this match {
     case Cons(h,t) => f(h(), t().foldRight(z)(f))
     case _ => z
   }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  def zip[B](s2: Stream[B]): Stream[(A,B)] = zipWith(s2)((_,_))
// The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results,
// which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is
// needed to compute the result. Here, we simply extract the accumulated list once finished.
//Stream(1,2,3,4,5)
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Stream.cons(b2, p1._2))
    })._2
//note main difference with zipAll is if the list are different size it keeps going filling
//missing value with None
def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))
 //this -> creates a tuple, e.g 1 -> "Hello" creates (Int,String) = (1,Hello) tuple 
 //note this also gives compile warning saying match
 //note only thing i can do with this function is this
 //Stream(1,2,3).zipWithAll(Stream(4,5,6))((x,y) => (x,y))
 //i think this is make just to work with zipAll
 def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), Stream.empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (Stream.empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
      case (_,_) => None //used to get rid of compile warning
    }
  //note here is a pattern match of a tuple (_._2) the 2 matches the right side of the tuple 
 def startsWith[A](s: Stream[A]): Boolean = zipAll(s).takeWhile(!_._2.isEmpty) forAll {
   case (a,b) => a == b
 }
 //Stream(1,2,3,4).tails.map(_.toList).toList
 def tails: Stream[Stream[A]] =
   Stream.unfold(this) {
     case Empty => None
     case s => Some(s,(s drop 1))
   } append Stream(Stream.empty)

 def hasSubsequences[A](s: Stream[A]): Boolean = 
   tails exists (_ startsWith s)

 def exists_1(p: A => Boolean): Boolean = foldRight(false)((a,b) => p(a) || b) 
  //Stream(2,4,6,8,10,12).forAll(x => (x % 2) == 0)
 def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b) 

 def headOption: Option[A] = foldRight(None: Option[A])((h,_) => Some(h)) 
  
 def takeWhile_1(p: A => Boolean): Stream[A] = 
   foldRight(Stream.empty[A])((h,t) =>
       if (p(h)) Stream.cons(h,t)
       else Stream.empty)

 def map[B](f: A => B): Stream[B] = 
   foldRight(Stream.empty[B])((a,b) => Stream.cons(f(a),b))
 //Stream(1,2,3,4,5).mapViaUnfold(x => 2*x).toList 
 def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

 def filter(f: A => Boolean): Stream[A] = 
   foldRight(Stream.empty[A])((h,t) =>
       if (f(h)) Stream.cons(h,t)
       else t)

 def append[B>:A](s: => Stream[B]): Stream[B] = 
   foldRight(s)((h,t) => Stream.cons(h,t)) 
//Stream(stream(1,2,3),Stream(4,5,6)).flatMap(x = x.map(_*2)).toList
//Stream(1,2,3).flatMap(x => Stream(2*x))
 def flatMap[B](f: A => Stream[B]): Stream[B] =
   foldRight(Stream.empty[B])((h,t) => f(h) append t)
 //Stream(1,2,3,4,5).find(x => x > 2)
 def find(p: A => Boolean): Option[A] = 
   filter(p).headOption


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  //to create a stream 
  //val x = Stream.cons(1,Stream(2,3,4))
  //or val x = Stream(1,2,3,4)
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  //Stream.constant(3).take(5).toList
  def constant[A](a: A): Stream[A] = cons(a,constant(a))
  //Stream.from(4).take(10).toList
  def from(n: Int): Stream[Int] = cons(n,from(n+1))
  //Stream.fibs(1,0).take(10).toList
  def fibs(p1: Int, p2: Int): Stream[Int] = cons(p2,fibs(p1+p2,p1))
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))
  //Stream.unfold(10)(x => if (x > 20) None else Some(x,x+2)).toList
  //Stream.unfold(1)(x => Some(x,2*x)).take(10).toList
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }  
  //Stream.fibsWithfold.take(10).toList
  def fibsWithUnfold: Stream[Int] = unfold((0,1)){case (f0,f1) => Some(f0,(f1,f0+f1))}
  
  def fromWithUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x,x+1))

  def constantWithUnfold[A](a:A): Stream[A] = unfold(a)(x => Some(x,x))

  def ones: Stream[Int] = unfold(1)(x => Some(x,x))
  /* this is my crap just to try to get zipWithAll to work with a function */
  def simpDoub(x: Option[(Int,Int)]): Option[(Int,Int)] = x match {
    case Some((a,b)) => Some((2*a,2*b))
    case _ => None
  }
  def simpDoub1(x: Option[(Int,Int)]): (Int,Int) = x match {
    case Some((a,b)) => (2*a,2*b)
    case _ => (0,0) 
  }
 def simpDoub2(x: (Option[Int],Option[Int])): (Int,Int) = x match {
    case (Some(a),Some(b))  => (2*a,2*b)
    case _ => (0,0) 
  }



}
