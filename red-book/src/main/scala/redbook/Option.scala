package redbook

enum Option[+A] {
  case Some(value: A)
  case None

  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(a => if f(a) then Some(a) else None)

  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

}

object Option {

  //Ex:4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(i => b.map(j => f(i, j)))

  //Ex:4.4
  def sequence[A](as: List[Option[A]]): Option[scala.List[A]] = List.foldRight(as, Some(Nil), (a, acc) => map2(a, acc)(_ :: _))
}