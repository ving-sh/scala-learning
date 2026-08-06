import redbook.LazyList
import redbook.LazyList._

def maybeTwice(i: => Int): Int = i + i

maybeTwice({
  println("!!"); 3
})

LazyList(1, 2, 3).take(2).toList

LazyList(1, 2, 3, 4, 5).takeWhile(_ < 3).toList

LazyList(1, 2, 3, 4, 5).takeWhileInFold(_ < 3)
LazyList(1, 2, 3, 4, 5).takeWhileInFold(_ < 4).toList

LazyList(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0)
LazyList(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList

val ones: LazyList[Int] = cons(1, ones)

ones.take(10).toList
continually(3).take(10).toList

from(21).take(10).toList

fibs.take(10).toList

import redbook.Option.Some
//Ex:5.12 fib in unfold
unfold((0,1)){case (i, j) =>
  Some((i, (j, i+j)))
}.take(10).toList

//Ex:5.12 continually
val fromUn = (n: Int) =>
  unfold(n)(i => Some((i,i+1)))

fromUn(21).take(10).toList

val oneU = unfold(())(_ => Some((1,())))

oneU.take(10).toList

val continuallyU = (n: Int) =>
  unfold(())(_ => Some((n,())))

continuallyU(2).take(10).toList

//Ex:5.13
from(1).takeViaUnfold(10).toList

ones.mapViaUnfold(_*3).take(5).toList

from(1).zipWithN("hi").takeViaUnfold(10).toList

from(1).takeViaUnfold(5).zipAll(from(1).takeViaUnfold(10)).toList
from(1).takeViaUnfold(10).zipAll(from(1).takeViaUnfold(5)).toList

LazyList(1,2,3).startsWith(LazyList (1,3))

LazyList(1,2,3).tails.toList.map(_.toList)

//Ex:5.16
LazyList(1, 2, 3).scanRight(0)(_ + _).toList

LazyList(1,2,3).scanRight(Empty)((i, acc) => cons(i, acc)).map(_.toList).toList

