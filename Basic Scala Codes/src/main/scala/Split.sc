//Split a list into two parts.
//The length of the first part is given. Use a Tuple for your result.
//Example:
//
//  scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

val l= List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

//Tuple2[List[Symbol], List[Symbol]]

def splitList(num:Int, l:List[Symbol], ind: Int=0, res:(List[Symbol],List[Symbol])=(Nil,Nil)) : (List[Symbol], List[Symbol]) = l match{
  case x::xs if ind<num => splitList(num,xs,ind+1,(res._1:+x,res._2))
  case x::xs if ind>=num => splitList(num,xs,ind+1,(res._1,res._2:+x))
  case Nil => res
}

splitList(3,l)