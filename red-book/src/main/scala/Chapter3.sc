import redbook.List
import redbook.List._

val ls = List(1,2,3,4,5)

List.drop(ls, 2)

List.setHead(10, ls)

//Ex: 3.8
List.foldRight(List(1, 2, 3), Nil: List[Int], Cons(_, _))

List.length(ls)

List.foldLeft(ls, 0, (acc, _)=> acc+1)

List.sumL(ls)

List.reverse(ls)

List.foldRightViaFoldLeft(List(1, 2, 3), Nil: List[Int], Cons(_, _))

List.append(List(1,2,3), List(4,5))

List.addOne(List(1,2,3))

List.doubleToString(List(2.3,2.44,3.14,4.9))

List.map(List(1,2,3), _+1)

List.map(List(2.3,2.44,3.14,4.9), _.toString)

List.flatMap(List(1, 2, 3), i => List(i,i))

List.filter(ls, _ != 2)

List.filterUsingFlatMap(ls, _ != 2)

List.addPairwise(List(1,2,3),List(4,5,6))

List.zipWith(List(1,2,3),List(4,5,6), (a, b)=> a+b)

List.take(ls, 4)

List.subFromStart(ls, List(1,2,3))
List.hasSubsequence(ls, List(1,2,3))
List.hasSubsequence(ls, List(3))
List.hasSubsequence(ls, List(3,4))
List.hasSubsequence(ls, List(2,3,7))


import redbook.Tree
import redbook.Tree._

val tree = Branch(Leaf(1), Branch(Branch(Leaf(2),Leaf(3)), Leaf(1)))

tree.max
tree.depth
tree.size

val tree2 = tree.map(_*3)

val sumOfTree = tree2.fold(i => i, (l,r)=>l+r)
val depthOfTree = tree2.fold(i=>1, (l,r)=> l+r)
val sizeOfTree = tree2.fold(i=>1, 1+_+_)
val maxOfTree = tree2.fold(i=>i, (l,r)=> if l>r then l else r)
