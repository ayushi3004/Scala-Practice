//find penultimate element of a list
val l=(1 to 10).toList

def findPen(l:List[Int]) : Int = l match{
  case Nil => throw new RuntimeException("can't find penultimate element")
  case x:: _ ::Nil => x
  case x::xs => findPen(xs)
}

findPen(l)
