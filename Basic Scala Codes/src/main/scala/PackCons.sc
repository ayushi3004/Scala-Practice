//Pack consecutive duplicates of list elements into sublists.
//If a list contains repeated elements they should be placed in separate sublists.
//  Example:
//
//  scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

val l:List[Symbol]=List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

def packCons(l:List[Symbol], inList:List[Symbol]=Nil, outList:List[List[Symbol]]=Nil) : List[List[Symbol]] = l match{
  case Nil => outList

  case x::Nil if inList==Nil => outList :+ List(x)

  case x::Nil if x==inList.head =>
    val fin = x::inList
    outList:+fin

  case x::Nil if x!=inList.head =>
    packCons(Nil,Nil,outList:+List(x))
    val fin = x::inList
    outList:+fin

  case x::xs if x==xs.head => packCons(xs,x::inList,outList)

  case x::xs if x!=xs.head =>
    val fin = x::inList
    packCons(xs,Nil,outList:+fin)
}

packCons(l)
packCons(List('x,'a))