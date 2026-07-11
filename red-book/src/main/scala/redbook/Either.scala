package redbook

import redbook.List.Cons

enum Either[+E, +A] {
  case Left(value: E)
  case Right(value: A)

  //Ex:4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(v) => Left(v)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(v) => Left(v)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => Right(v)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
//    this.flatMap(a => that.map(b => f(a, b)))
    for{
      a <- this
      b <- that
    } yield f(a,b)
  }
}

object Either {

  //Ex:4.7
  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] = traverse(as)(a => a)
//    List.foldRight(as, Right(List.Nil), (a, acc)=> a.map2(acc)(Cons(_,_)))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    List.foldRight(as, Right(List.Nil), (a, acc)=> f(a).map2(acc)(Cons(_,_)))

}
