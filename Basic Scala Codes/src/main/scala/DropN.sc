//Drop every Nth element from a list.
//  scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

val l =  List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

//implicit approach
//
//val drop= (
//            (num:Int, l:List[Symbol]) =>
//              l.filter( (i:Symbol) => (l.indexOf(i)+1) % num !=0
//            )
//          )
//drop(3,l)
//
////explicit approach
//val drop:(Int,List[Symbol]) => List[Symbol] =
//  (num,l) => l.filter( (i:Symbol) => (l.indexOf(i)+1) % num !=0)
//drop(3,l)

val drop2 : (Int, List[Symbol], Int) => List[Symbol] ={(num, l, ind)=> l match {
  case x::xs if ind%num==0 => drop2(num,xs,ind+1)
  case x::xs if ind%num!=0 => x::drop2(num,xs,ind+1)
  case Nil => Nil
  }
}

drop2(7,l,1)