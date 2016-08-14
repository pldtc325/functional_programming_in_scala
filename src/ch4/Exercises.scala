package ch4

object Other {
  def lift[A, B](f:A=>B):Option[A] => Option[B] = {
    optionA:Option[A] => optionA.map(f)
  }

  def lift2[A, B, C](f:(A,B)=>C):(Option[A], Option[B]) => Option[C] = {
    (optionA:Option[A], optionB:Option[B]) => optionA.map2(optionB)(f)
  }
}

trait Option[+A] {
  def map[B](f:A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  def map2[B,C](b:Option[B])(f: (A, B) => C):Option[C] = {
    flatMap(aa => (b map (bb => f(aa,bb))))
  }

  def flatMap[B](f:A=>Option[B]):Option[B] = {
    map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: =>B):B = {
    this match {
      case None => default
      case Some(a) => a
    }
  }
}

case class Some[A](get:A) extends Option[A]
object None extends Option[Nothing]

object Exercises extends App {
  val f = (a:Int) => a + 1
  val fLifted = Other.lift(f)
  val r = fLifted(Some(1)).getOrElse(0)
  println(r)

  val f2 = (a:Int, b:Int) => a + b
  val f2Lifted = Other.lift2(f2)
  val r2 = f2Lifted(Some(1), Some(2)).getOrElse(0)
  println(r2)

}
