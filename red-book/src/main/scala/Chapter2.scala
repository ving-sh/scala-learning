import scala.annotation.tailrec


object Chapter2 {

  //Exercise: 2.1
  def fib(n: Int): Int = {
    @tailrec
    def loop(i: Int, prev: Int, curr: Int): Int = {
      if i >= n then prev
      else loop(i + 1, prev, prev+curr)
    }
    loop(0,0,1)
  }

  def factorial(n: Int): Int = {
    @tailrec
    def loop(i: Int, acc: Int): Int = {
      if i == n then acc
      else loop(i+1, acc*(i+1))
    }
    loop(1,1)
  }

  //Exercise: 2.2
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    val length = as.length
    @tailrec
    def loop(n: Int): Boolean = {
      if n >= length-1 then true
      else if gt(as(n), as(n+1)) then false
      else loop(n+1)

    }
    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a,b)

  // Exercise: 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a,b)

  //Exercise: 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a,b) => f(a)(b)

  //Exercise:2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def main(arg: Array[String]): Unit = {
    println(fib(10))
    println(factorial(10))

    println("---- Excercise:2.2 ----")
    println(isSorted(Array(1, 2, 3), _ > _))
    println(isSorted(Array(1, 2, 1), _ > _))
    println(isSorted(Array(3, 2, 1), _ < _))
    println(isSorted(Array(1, 2, 3), _ < _))
    println("---------")
  }

}