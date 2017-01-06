import scala.{Option => _, Either => _, _}
:paste
sealed trait Either[+E,+A] {
  //Right(3).map(_*3)
  //Left(2).map((o:Int) => 3*o)
  //(Right(3) map (x => 2*x))
  def map[B](f: A => B): Either[E,B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }
  //Right(3).flatMap(x => Right(2*x))
  //Left(3).flatMap((x:Int) => Right(2*x))
  //when mapping other right side we have to promote the left type param
  //to some supertype, to satisfy the +E variance annotation
  def flatMap[EE >: E,B](f: A => Either[EE,B]): Either[EE,B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }
  //Right(3).orElse(Left(2))
  //Left(1).orElse(Left(2))
  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }
  //the for must lift the Either value ?
  //
  //Right(3).map2(Right(4))((x,y) => x*y)
  //Left(3).map2(Right(4))((x:Int,y:Int) => x*y)
  //(Right(3) map2 Right(4))((x,y) => x*y)
  //(Right(3) map2 Right(List(4,5,6)))(_ :: _)
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): 
   Either[EE, C] = for { a <- this; b1 <- b } yield f(a,b1)
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either
{
  def sequence[E,A](es: List[Either[E,A]]): Either[E, List[A]] =
    traverse(es)(o => o)
  //Either.traverse(List(1,2,3))(x => Right(2*x))
  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] = 
    as match {
      case Nil => Right(Nil)
      case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
    }
}
