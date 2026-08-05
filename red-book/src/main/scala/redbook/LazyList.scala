package redbook

import redbook.LazyList.{cons, empty, unfold}
import redbook.Option
import redbook.Option.*

import scala.List
import scala.annotation.tailrec

enum LazyList[+A] {
  case Empty
  case Con(h: () => A, t: () => LazyList[A])

  def heapOption: Option[A] = this match {
    case Empty => None
    case Con(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Con(h, t) => h() :: t().toList
  }

  def take(n: Int): LazyList[A] = this match {
    case Con(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Con(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): LazyList[A] = this match {
    case Con(h, t) if n >= 1 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): LazyList[A] = this match {
    case Con(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match {
    case Con(h, t) => f(h(), t().foldRight(acc)(f))
    case Empty => acc
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  //Ex:5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  //Ex:5.5
  def takeWhileInFold(p: A => Boolean): LazyList[A] = foldRight(empty[A])((a, acc) => if p(a) then cons(a, acc) else empty)

  //Ex:5.6
  def headOption: Option[A] = foldRight(Option.None)((a, _) => Option.Some(a)) //`Option.` is to use redbook.Option

  //Ex:5.7
  def map[B](f: A => B): LazyList[B] = foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def filter(p: A => Boolean): LazyList[A] = foldRight(empty[A])((a, acc) => if p(a) then cons(a, acc) else acc)

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] = foldRight(that)((a, acc) => cons(a, acc))

  //Ex:5.13
  def mapViaUnfold[B](f: A => B): LazyList[B] = unfold(this) {
    case Con(h, t) => Some(f(h()), t())
    case _ => None
  }

  def takeViaUnfold(n: Int): LazyList[A] = unfold((n, this)) {
    case (i, Con(h, t)) if i > 0 => Some((h(), (i - 1, t())))
    case _ =>  None
  }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] = unfold(this) {
    case Con(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWithN[B](n: B): LazyList[(A, B)] = unfold(this) {
    case Con(h, t) => Some((h(), n), t())
    case _ => None
  }

  def zipWith[B](that: LazyList[B]): LazyList[(A, B)] = unfold((this, that)) {
    case (Con(h1, t1), Con(h2, t2)) => Some((h1() -> h2(), t1() -> t2()))
    case _ => None
  }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] = unfold((this, that)) {
    case (Empty, Empty) => None
    case (Con(h1, t1), Con(h2, t2)) => Some(Some(h1()) -> Some(h2()), t1() -> t2())
    case (Con(h, t), Empty) => Some((Some(h()) -> None, t() -> Empty))
    case (Empty, Con(h, t)) => Some((None -> Some(h()), Empty -> t()))
  }

  def startsWith[B >: A](prefix: LazyList[B]): Boolean = {
    @tailrec
    def go(one: LazyList[A], two: LazyList[B]): Boolean = (one, two) match {
      case (Con(_, _), Empty) => true
      case (Con(h1, t1), Con(h2, t2)) if h1() == h2() => go(t1(), t2())
      case _ => false
    }
    go(this, prefix)
  }

  def tails: LazyList[LazyList[A]] = unfold(this){
    case Con(h, t) => Some(cons(h(), t()), t())
    case _ => None  
  }
}

object LazyList {
  def cons[A](
               h: => A,
               t: => LazyList[A]
             ): LazyList[A] =
    lazy val head = h
    lazy val tail = t
    Con(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] = {
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail *))
  }

  //Ex:5.8
  def continually[A](a: A): LazyList[A] = {
    lazy val single: LazyList[A] = cons(a, single)
    single
  }

  //Ex:5.9
  def from(n: Int): LazyList[Int] = {
    lazy val element: LazyList[Int] = cons(n, element.map(_+1))
    element
  }

  //Ex:5.10
  def fibs: LazyList[Int] = {
    def go(current: Int, next: Int): LazyList[Int] = cons(current, go(next, current + next))
    go(0,1)
  }

  //Ex:5.11
  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = {
    import redbook.Option._
    f(state) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Empty
    }
  }

}
