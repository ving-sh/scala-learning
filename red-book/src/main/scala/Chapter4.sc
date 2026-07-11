def failingFn(i: Int): Int =
  val y: Int = throw Exception("fail!")
  try
    val x = 42 + 5
    x + y
  catch
    case e: Exception =>  43

def failingFn2(i: Int): Int =
  try
    val x = 42 + 5
    x + ((throw Exception("fail!")): Int)
  catch
    case e: Exception => 43

import redbook.Option
import redbook.Option._
import redbook.List

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m,2))))
    
variance(Seq(1,2,3,4,5))

map2(Some(2), Some(3))(_+_)
map2(Some(2), None:Option[Int])(_+_)
map2(None:Option[Int], Some(3))(_+_)

sequence(List(Some(1), Some(2), Some(3)))
sequence(List(Some(1), Some(2), None))


//Ex:4.4
traverse(List(2,4,6))(x => if x%2 == 0 then Some(x/2) else None)

//sequence in terms of traverse
traverse(List(Some(1), Some(2), Some(3)))(identity)
traverse(List(Some(1), Some(2), None))(identity)

import redbook.Either
