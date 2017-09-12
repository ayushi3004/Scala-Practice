//find last element of list
val l=(1 to 10).toList

def findLast(l : List[Int]):Int = l match {
  case Nil => throw new RuntimeException("Cant find last in empty")
  case x :: Nil => x
  case x::xs => findLast(xs)
}

findLast(l)
findLast(List())