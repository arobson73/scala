import scala.{Option => _ , Some => _, None => _, _}
:paste
sealed trait Option[+A]
{
  //Some(3).map(_*2)
  //None.map((o: Int) => (2*o))
  def map[B](f: A => B): Option[B] = this match {
    case None => None 
    case Some(a) => Some(f(a)) 
  }
  //Some(3).getOrElse(Some(4))
  //None.getOrElse(Some(3))
  //Some(3).getOrElse(4)
  //None.getOrElse(3)
  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  //Some(3).flatMap(x => Some(x*2))
  //note here the map(f) will do this if this is supplied
  //Some(3).map(x => Some(x*2)) gives Some(Some(6))
  //the getOrElse then un-Somes it to gives Some(6)
  //None.flatMap((o: Int) => Some(o*2))
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None
  //Some(3).flatMap_1(x => Some(x*2))
  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }
  //Some(3).orElse(Some(2))
  //None.orElse(Some(3))
  //note after orElse must be wrapped in Option compared to getOrElse
  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def orElse_1[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }
  //Some(4).filter(_ > 3)
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
  /*
  def flatMap[B](f: A => Option[B]): Option[B] = sys.error("todo")

  def orElse[B>:A](ob: => Option[B]): Option[B] = sys.error("todo")

  def filter(f: A => Boolean): Option[A] = sys.error("todo")
  */
}
case class Some[+A](get:A) extends Option[A]
case object None extends Option[Nothing]

object Option 
{
  //Option.mean(Seq(1,2,3,4))
  def mean(xs: Seq[Double]): Option[Double] = 
    if(xs.isEmpty) None else Some(xs.sum / xs.length)
  //note how to get this to return None in event of fail?
  def variance(xs:Seq[Double]): Option[Double] = 
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x-m,2))))
  // map2(Some(3),Some(4))((x,y) => x*y) 
  // map2(Some(3),None)(x:Int,y:Int) => x*y)
  // map2(Some(1),Some(List(2,3,4)))(_ :: _) 
  // above will generate Some(List(1,2,3,4))
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa,bb)))
  //this will use flatMap to unwrap the Option, passing each unwrapped element
  //to the map which concats them. The outer flatmap then applies the Option
  //wrappet to the list of elements
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
    //map2(f(h),map2(f(i),map2(f(j),Some(Nil))(_ :: _))
  //here replace traverse with everything in traverse. see map2 above for map2 with List 
  //on RHS. the recursive traverse will build up a Some(List(f(a),f(b))) for example. 
  //Remeber the map removes the Option after f(a), then wraps it in Option after transforming
  //the list
  //traverse(List(1,2,3))(x => Some(x*2))
  //traverse(List(Some(1),Some(2)))(x => x) 
  //this produces Some(List(1,2))
  //note in this case the traverse just maps maps each Option value in the list
  //to the same Option value due to the (x => x) function. inside the traverse
  //map2 is used. the function with map2 is (_ :: _) which removes the Option value
  def traverse[A,B](a:List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t =>  map2(f(h),traverse(t)(f))(_ :: _)
    }
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
 //look up the std lib for foldRight. Here we specify the output type, and 
 //start with Some(Nil), we then apply map2 , the left side being updated with the 
 //function and the right side is the accumulator, both of with we append
  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

}
