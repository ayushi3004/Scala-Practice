//Flatten a nested list structure.
//Example:
//  scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
//res0: List[Any] = List(1, 1, 2, 3, 5, 8)

val l=List(List(1, 1), 2, List(3, List(5, 8)))

def flatten2(l:List[Any]):List[Any]={
  l.flatMap(_ match {
    case l1:List[Any] => flatten2(l1)
    case s:Int => List(s)
  })
}
flatten2(l)