//Duplicate the elements of a list.
//Example:
//  scala> duplicate(List('a, 'b, 'c, 'c, 'd))
//res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
val l=List('a, 'b, 'c, 'c, 'd)

def duplicate(l:List[Any]): List[Any] ={
  l.flatMap(List.fill(2)(_))
}
duplicate(l)
