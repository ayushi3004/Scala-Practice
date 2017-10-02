//Rotate a list N places to the left.
//Examples:
//  scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
//
//scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)

val l =  List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

def rotate(places:Int, li: List[Symbol]) : List[Symbol] = {
  // necessary if places=0 or >l.size
  val n = if(li.isEmpty) 0 else places%li.size

  if(places < 0 )
    rotate(n + li.size, li)
  else
    li.drop(n) ::: li.take(n)
}

rotate(3, l)
