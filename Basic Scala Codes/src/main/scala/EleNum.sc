//Find the number of elements of a list.
//Example:
//  scala> length(List(1, 1, 2, 3, 5, 8))
//res0: Int = 6

val l= List(1, 1, 2, 3, 5, 8)

def findLength[T](l:List[T],cnt:Int=0):Int = l match{
  case x::xs => findLength(xs,cnt+1)
  case Nil => cnt
}

findLength(l)