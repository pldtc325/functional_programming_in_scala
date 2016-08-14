package ch2

/**
  * Created by pltc325 on 16/8/5.
  */
object Ch2 extends App {
  def e_2_1(n:Int):Int = {
    def fib(n:Int): Int = {
      if(n == 0) 0
      else if(n==1) 1
      else fib(n-1) + fib(n-2)
    }
    fib(n)
  }

  println(e_2_1(0))
  println(e_2_1(1))
  println(e_2_1(2))
  println(e_2_1(3))
  println(e_2_1(4))
}
