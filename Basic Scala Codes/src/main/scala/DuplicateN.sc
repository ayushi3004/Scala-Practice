//Duplicate the elements of a list a given
// number of times.
//scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
//res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)

val l= List('a, 'b, 'c, 'c, 'd)

val duplicateN:(Int,List[Symbol])=>List[Symbol]=
  (num,l)=> l.flatMap(List.fill(num)(_))

duplicateN(3,l)
