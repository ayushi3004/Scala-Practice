//Extract a slice from a list.
//Given two indices, I and K, the slice is the list containing the elements from and
//  including the Ith element up to but not including the Kth element of the original list.
//Start counting the elements with 0.
//scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: List[Symbol] = List('d, 'e, 'f, 'g)

val l= List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

def slice(s:Int, e:Int, l:List[Symbol]): List[Symbol] ={
  l.filter( (i:Symbol) => (
    l.indexOf(i) >=s && l.indexOf(i)<e)
  )
}
slice(3,7,l)