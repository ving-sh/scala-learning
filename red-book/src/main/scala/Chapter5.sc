import redbook.LazyList

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