package redbook

import scala.annotation.tailrec

enum List[+A] {
  case Nil
  case Cons(head: A, tail: List[A])
}

object List {
  def apply[A](ls: A*): List[A] =
    if ls.isEmpty then Nil
    else Cons(ls.head, apply(ls.tail*))

  def sum(inits: List[Int]): Int = inits match
    case Nil => 0
    case Cons(h, t) => h + sum(t)


  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)

  def tail[A](ls: List[A]): List[A] = ls match
    case Cons(_, tl) => tl
    case Nil => sys.error("tail of empty list")

  //Ex: 3.3
  def setHead[A](head: A, ls: List[A]): List[A] = ls match
    case Nil => sys.error("empty List's head cannot be replaced")
    case _  => Cons(head, tail(ls))

  // Ex: 3.4
  @tailrec
  def drop[A](ls: List[A], n: Int): List[A] = ls match {
    case xs if n == 0 =>  xs
    case Cons(_, t) if n > 0 => drop(t, n-1)
    case Nil => Nil
    case _ if n < 0 => sys.error("cannot drop negative int")
  }

  //Ex: 3.5
  @tailrec
  def dropWhile[A](ls: List[A], f: A => Boolean): List[A] = ls match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => ls
  }

  //Ex:3.6
  def init[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("empty list")
    case Cons(x, Nil) => Nil
    case Cons(hd, tl) => Cons (hd, init(tl))
  }

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = as match {
    case Nil => acc
    case Cons(x, xs) => f(x,foldRight(xs, acc, f))
  }

  //Ex:3.9
  def length[A](as: List[A]): Int = foldRight(as, 0, (_, acc)=> acc+1 )

  //Ex:3.10
  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B = as match {
    case Nil => acc
    case Cons(x, xs) => foldLeft(xs, f(acc, x), f)
  }

  //Ex:3.11
  def sumL(as: List[Int]): Int = foldLeft(as, 0, (acc, i) => acc+i)

  def productL(as: List[Int]): Int = foldLeft(as, 1, (acc, i)=> acc*i)

  //Ex: 3.12
  def reverse[A](as: List[A]):List[A] = foldLeft(as, Nil, (acc, i) => Cons(i, acc))

  //Ex: 3.13
  def foldRightViaFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(as, (b:B) => b, (g:B => B, a:A)=> (b:B) => g(f(a,b)))(acc)
//    foldLeft(reverse(as), acc, (acc, i) => f(i,acc))

  //Ex: 3.14
  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2, Cons(_,_))

  //Ex:3.15
  def concat[A](as: List[List[A]]): List[A] = foldRight(as, Nil, append)

  //Ex:3.16
  def addOne(as: List[Int]): List[Int] = foldRight(as, Nil, (i, acc) => Cons(i+1, acc))

  //Ex:3.17
  def doubleToString(as: List[Double]): List[String] = foldRight(as, Nil, (i, acc) => Cons(i.toString, acc))

  //Ex:3.18
  def map[A, B](as: List[A], f: A => B): List[B] = foldRight(as, Nil, (i,acc) => Cons(f(i),acc))

  //Ex:3.19
  def filter[A](as: List[A], f: A => Boolean): List[A] = foldRight(as, Nil, (i, acc) => if f(i) then Cons(i,acc) else acc)

  //Ex:3.20
  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] = concat(map(as,f))

  //Ex: 3.21
  def filterUsingFlatMap[A](as: List[A], f:A => Boolean): List[A] = flatMap(as, i=> if f(i) then List(i) else Nil)

  //Ex:3.22
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Cons(x1, t1), Cons(x2, t2)) => Cons(x1+x2, addPairwise(t1,t2))
    case (Nil, Nil) => Nil
    case _ => sys.error("Lists are not pairs")
  }

  //Ex:3.23
  def zipWith[A,B,C](a: List[A], b: List[B], f:(A,B)=> C): List[C] = (a,b) match {
    case (Cons(x1, t1), Cons(x2, t2)) => Cons(f(x1,x2), zipWith(t1,t2,f))
    case (Nil, Nil) => Nil
    case _ => sys.error("Lists are not pairs")
  }

  //Ex:3.24
  def take[A](as: List[A], n: Int): List[A] = as match
    case Cons(h, t) if n > 0 => Cons(h, take(t, n-1))
    case _ if n == 0 => Nil
    case _ => sys.error("negative number not required")
  
  def subFromStart[A](a: List[A], b: List[A]): Boolean = take(a, length(b)) == b

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 && subFromStart(sup, sub) => true
    case (Cons(h1, t1), Cons(h2, t2)) => hasSubsequence(t1, sub)
    case _ => false

}