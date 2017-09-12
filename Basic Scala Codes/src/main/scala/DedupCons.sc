//Eliminate consecutive duplicates of list
// elements.
//If a list contains repeated elements they
// should be replaced with a single copy of
// the element. The order of the elements should
// not be changed.
// scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

val l= List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)


def dedup[Int](l: List[Int]):List[Int] = l match {
  case Nil => Nil
  case x::Nil => List(x)
  case x::xs if (x == xs.head) => dedup(xs)
  case x::xs => x::dedup(xs)
}
dedup(l)