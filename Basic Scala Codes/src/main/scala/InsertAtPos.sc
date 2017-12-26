//P21 (*) Insert an element at a given position into a list.
//  Example:
//  scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
//res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)

val l: List[Symbol] = List('a, 'b, 'c, 'd)

def insertAt(item: Symbol, pos: Int, list: List[Symbol]) = {
  (list.take(pos) :+ item) ::: list.drop(pos)
}

insertAt('new, 1, l)