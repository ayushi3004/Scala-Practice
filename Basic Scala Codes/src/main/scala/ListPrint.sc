//print elements in the list using recursion
val l=(1 to 10).toList

def printList(l: List[Int]): Unit =l match{

  case x::xs =>
    println(x)
    printList(xs)
  case _ =>
}

printList(l)