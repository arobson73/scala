//:paste
trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}
//RNG.Simple(33).nextInt
//also i think this nextInt is seems biased to large numbers
object RNG {
  // NB - this was called SimpleRNG in the book text
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  //note using rng var inside here looks confusing at first, but
  //its because the Rand[B] type is RNG => (A,RNG). hence
  //that is the pattern we use inside the function, the rng corresponds to the
  //RNG, the stuff inside the braces corresponds to the (A,RNG) output.
  //inside the braces we also use s and f, that are passed in
  //val(i,r) = RNG.map(RNG.nonNegativeInt)(x => 2*x)(RNG.Simple(4))
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }
 //val simp = RNG.Simple(3)
  //val rng = RNG.nextInt._2
  //val (i,rng2) = RNG.nonNegative(rng)
  //
  //better way 
  //val simpr = RNG.Simple(4)
  //val (i,r) = nonNegative(simpr)
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i,r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = 
  {
    val (i,r) = nonNegativeInt(rng)
    (i/(Int.MaxValue.toDouble + 1),r)
  }
  //val ((i,d),rng2) = intDouble(rng)
  def intDouble(rng: RNG): ((Int,Double), RNG) = 
  {
    val (i,r1) = rng.nextInt
    val (d,r2) = double(r1)
    ((i,d),r2)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = 
  {
    val ((i,d),rng2) = intDouble(rng)
    ((d,i),rng2)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = 
  {
    val (d1,r1) = double(rng)
    val (d2,r2) = double(r1)
    val (d3,r3) = double(r2)
    ((d1,d2,d3),r3)
  }
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
  {
    if( count == 0)
      (List(),rng)
    else
    {
      val (i,rng2) = nonNegativeInt(rng)
      val (ii,rng3) = ints(count-1)(rng2)
      (i::ii,rng3)
    }
  }

// A tail-recursive solution
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count == 0)
        (xs, r)
      else {
        val (x, r2) = r.nextInt
        go(count - 1, r2, x :: xs)
      }
    go(count, rng, List())
  }

  //val simpr = RNG.Simple(33)
  // val(d,rng) = doubleWithmap(simpr)
  val doubleWithmap: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble+1))
  //RNG.map2(RNG.nonNegativeInt,RNG.double)((x,y) => (x-1,y+2.0))(RNG.Simple(4))
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
          val (i,rng2) = ra(rng)
          val (d,rng3) = rb(rng2)
          (f(i,d),rng3)
    }
  //RNG.both(RNG.nonNegativeInt,RNG.double)(RNG.Simple(4))
  //recall the Rand[C] signifies the output type which
  //in this case is (A,B)
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra,rb)((_,_))
  
  //RNG.randIntDouble(RNG.Simple(8))
  val randIntDouble: Rand[(Int,Double)] = both(int,double)

  val randDoubleInt: Rand[(Double,Int)] = both(double,int)
  //RNG.sequence(List(RNG.int,RNG.int))(RNG.Simple(6))
  //RNG.sequence(List(RNG.nonNegativeInt(_),RNG.nonNegativeInt(_)))(RNG.Simple(7))
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
   //RNG._ints(10)(RNG.Simple(9))
  def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(RNG.int))

  //think of g(a) have type Rand[B] which is (RNG => (B, RNG))
  //hene i need to pass the r1 to get the correct output i think
  //RNG.flatMap(nonNegativeInt){i => RNG.unit(2*i)}(RNG.Simple(9))
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1) // We pass the new state along
    }
  def mapWithflatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(x => map(rb)(y => f(x, y)))
}
import State._
//State((x:Int) => (1,x+1))
//note in general when we create this class we will
//always have to provide the run function.
//in order to get the first state we just call the run
//method. the run method requires a input initial state.
//this will return the new state and the value. 
//to get the value we can just say State((x:Int) => (x.toString,x+1)).run(1)._1
//
//if you wonder why map uses unit method (which usually create a type [Nothing,Int]
//when run like unit(4) for example. but since flatMap is defined as State[S,B]
//then the state type will be S.
//more useful State example 
//def f(rng:RNG):(Int,RNG) = rng.nextInt
//State(f).run(Simple(1))
case class State[S,+A](run: S => (A, S)) {
  //State((x:Int) => (10,x+1)).map(_*10).run(1) 
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))
  //val s1 = State((x:Int) => (10,x+10))
  //val s2 = State((x:Int) => (20,x+10))
  //s1.map2(s2)((t1,t2) => t1*t2).run(1)  
  //results in (200,21) // hence vals are multiplied and state is x + 20
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
  //val x = State((x:Int) => (0,x+1))
  //x.see.run(1) gives (2,2), the print show the 0
  def see: State[S,S] = State(s => {
    val (a,s1) = run(s)
    println(a)
    (s1,s1)
  })
  //x.see2(1) , the print shows zero
  def see2(s:S ) = {
    val (a,s1) = run(s)
    println(a)
  }
 }
object State {
  type Rand[A] = State[RNG, A]
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
   // The idiomatic solution is expressed via foldRight
   // first create list of states
   //val s1 = State((x:Int) => (10,x+10))
   //val s2 = State((x:Int) => (20,x+10))
   // val ls = List(s1,s2)
   // val x = State.sequenceViaFoldRight(ls).run(1)
   // results in (List[Int], Int) = (List(10, 20),21)
   // note the map / map2 apply the functions over the values, not the states
  def sequenceViaFoldRight[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

// This implementation uses a loop internally and is the same recursion
  // pattern as a left fold. It is quite common with left folds to build
  // up a list in reverse order, then reverse it at the end.
  // (We could   also use a collection.mutable.ListBuffer internally.)
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }

  // We can also write the loop using a left fold. This is tail recursive like the
  // previous solution, but it reverses the list _before_ folding it instead of after.
  // You might think that this is slower than the `foldRight` solution since it
  // walks over the list twice, but it's actually faster! The `foldRight` solution
  // technically has to also walk the list twice, since it has to unravel the call
  // stack, not being tail recursive. And the call stack will be as tall as the list
  // is long.
  def sequenceViaFoldLeft[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))
  //State.modify((x:Int) => x*100).run(1)
  //
  //think of get here in this for comprehension as State.get.flatMap(s => 
  //flatMap normally takes the "value" from the state type and applys a function to it.
  //Since get function replaces the value with the state itself, then this
  //time flatMap actually recieves the state. hence this is why f(s) can be
  //input to the set, we modify the state after its first been changed by f with set.
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

 // def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
  //State.get.run(1) gives (1,1)
  //State.get gives State[Nothing,Nothing]
  //now on a state i can invoke the run method and pass it a value (its inital state)
  //i get back from State.get.run(1) a (1,1)
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))  
  //A.R
  def show[S]:State[S,S] = for { a <- get} yield (a)
 // def show2[S]:State[S,S] = for { a <- get} yield (a,a.productArity)
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)
object Candy {
  //update is function that takes input and outputs a (machine => machine)
  //e.g Input => (Machine => Machine)
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs map (State.modify[Machine] _ compose update))
    s <- State.get
  } yield (s.coins, s.candies)
}
