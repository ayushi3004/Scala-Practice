//reverse a list

val l=(1 to 10).toList

//using func

def reverse(l: List[Int]) : Unit=l match{
  case x::xs => {
    reverse(xs)
    println(x)
  }
  case _ =>
}
reverse(l)

//using fold

l.foldLeft(List.empty[Int]){ (b, a) => a :: b }
