package ch6

trait RNG {
  def nextInt:(Int, RNG)
}

/**
  * In this chapter, we learned how to work with state
  * The func
  */

case class SimpleRNG(seed:Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val newRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, newRNG)
  }
}



object Ch6 extends App {
  val s1 = SimpleRNG(42)
  val (n1, s2) = s1.nextInt
  println("n1:"+n1)
  val (n2, s3) = s2.nextInt
  println("n2:"+n2)

  // this new type generalize the common behavior that
  // given an Object, return the output of the Object's method and a new Object with a new state
  // and how the object method give the output and how the object change its state will be given
  // when you initialize the variable of such type
  // e.g. val aRandA:Rand[Int] = unitRand _


  type Rand[+A] = RNG => (A, RNG)

  // map doesn't return (B, Rand[B])
  // the purpose of map is to just modify value of A by applying f to a, a.k.a f(a)
  def map[A,B](r:Rand[A])(f:A=>B):Rand[B] = {
    rng => {
      val (n, newrng) = r(rng)
      (f(n), newrng)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    (rng) => {
      val (n, rngNew) = ra(rng)
      val (m, rngNew2) = rb(rngNew)
      (f(n,m), rngNew2)
    }
  }

  // this is an implementation that conforms the paradigm of Rand[+A]
  def unitRand(rng: RNG):(Int, RNG) = {
    return rng.nextInt
  }

  // this function will double whatever unitRand generates
  // the double logic is passed to map's parameter f
  def doubleUnitRand[A, B](unitRand:Rand[Int]):Rand[Int] = {
    map(unitRand)(x=>2*x)
  }

  val (doubleN, s4) = doubleUnitRand(unitRand)(s1)

  println("doubleN:" + doubleN)

  // let's first look at the definition of map
  // def map[A,B](r:Rand[A])(f:A=>B):Rand[B]
  // and compare to flatMap, the only difference is that
  // f is A => B in map and is A => Rand[B] in flatMap
  // what is the difference?
  // in case of f: A => B, we are so sure that given any A, B is always desired,
  // if B is undesired, we therefore don't have a second chance
  // but if f: A => Rand[B], things are different
  // see positiveIntUsingMap and positiveIntUsingFlatMap for details
  def flatMap[A,B](r: Rand[A])(f: A => Rand[B]): Rand[B] = {
    rng:RNG => {
      val (n, rng2) = r(rng)
      f(n)(rng2)
    }
  }


  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def flatMap2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => Rand[C]): Rand[C] = {
    rng:RNG => {
      val (n, rng2) = ra(rng)
      val (m, rng3) = rb(rng2)
      f(n,m)(rng3)
    }
  }

  val int = unitRand _

  def positiveIntUsingMap: Rand[Int] = {
    // attention to map(int) { ... }
    // the { ... } part is the implementation of f parameter in map function
    map(int) { i =>
      // there is a problem here
      // Int.MinValue doesn't have an abs, we need to do a randomization again
      // but with the signature of f: A => B, this is impossible
      if (i != Int.MinValue) i.abs else ???
    }
  }

  def positiveIntUsingFlatMap: Rand[Int] = {
    flatMap(int) { i =>
      if (i != Int.MinValue) {
        rng:RNG => {(i.abs, rng)}
      }
      else {
        // thanks to flatMap's f: A => Rand[B]
        // the return type of f is as same as the positiveIntUsingFlatMap's
        // so we can return positiveIntUsingFlatMap inside f
        // which solve our Int.MinValue problem
        positiveIntUsingFlatMap
      }
    }
  }

  def map_bis[A, B](r:Rand[A])(f:A=>B):Rand[B] = {
    // we need to transform f: A => B to A => Rand[B]
    // that is to transform B to Rand[B]
    // knowing that Rand[B] is in fact RNG => (B, RNG)
    flatMap(r)(a => {rng => {(f(a), rng)}})
  }

  def map2_bis[A, B, C](ra:Rand[A], rb:Rand[B])(f:(A, B) => C):Rand[C] = {
    flatMap2(ra, rb)((a, b) => {rng => (f(a,b), rng)})
  }

  // make map more general, free from state type
  // def map[A, B](r:Rand[A])(f:A=>B):Rand[B] = null
  def mapGeneral[A, B, S](s: S => (A, S))(f: A => B): S => (B, S) = null

  // a more concise writing
  type State[S, +A] = S => (A, S)
  // if class is your taste, see the following detailed class StateClass
  case class StateClass[S, +A](run: S => (A,S))

}

/**
  * if we want to use a class to represent a function, a good way is to
  * use a property to contain your function
  * after that you can substitute your function variable with the class instance
  * @param run the contained function
  * @tparam S
  * @tparam A
  */
import State._

case class State[S, +A](run: S => (A,S)) {
  def map[B](f: A=>B):State[S, B] = {
    flatMap(a => unit(f(a)))
  }

  def flatMap[B](f: A => State[S, B]):State[S, B] = {
    new State( s => {
        val (r, s1) = run(s)
        f(r).run(s1)
      }
    )
  }
}

object State {
  def unit[S, A](a:A):State[S, A] = {
    State(s => (a, s))
  }

  /**
    * should not modify current state
    *        return the current state
    * @tparam S
    * @return
    */
  def get[S]():State[S, S] = State(s => (s, s))

  def set[S](s:S):State[S, Unit] = State(_ => ((), s))

//  def modify[S](f: S => S): State[S, Unit] = for {
//    s <- get
//    _ <- set(f(s))
//  } yield
}


