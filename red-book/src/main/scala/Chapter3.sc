import redbook.List
import redbook.List._

val ls = List(1,2,3,4,5,6)

List.drop(ls, 2)

List.setHead(10, ls)

List.foldRight(List(1, 2, 3), Nil: List[Int], Cons(_, _))

List.length(ls)

List.foldLeft(ls, 0, (acc, _)=> acc+1)

List.sumL(ls)
