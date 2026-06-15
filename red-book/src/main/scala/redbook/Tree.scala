package redbook

enum Tree[+A] {
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size
  }

  //Ex:3.26
  def depth: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => (l.depth max r.depth) + 1
  }

  //Ex:3.27
  def map[B](f:A => B): Tree[B] = this match {
    case Leaf(i) => Leaf(f(i))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }


}

object Tree {
  extension (t: Tree[Int])
    def firstPositive: Int = t match {
      case Leaf(i) => i
      case Branch(l, r) =>
        val lpos = l.firstPositive
        if lpos > 0 then lpos else r.firstPositive
    }

    //Ex:3.25
    def max: Int = t match {
      case Leaf(i) => i
      case Branch(l,r) => l.max max r.max
    }
}


