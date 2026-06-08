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
  def reverse[A](as: List[A]):List[A] = ???
}