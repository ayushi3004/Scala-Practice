//Find the Kth element of a list.
//  By convention, the first element in the list is element 0.
//Example:
//
//  scala> nth(2, List(1, 1, 2, 3, 5, 8))
//res0: Int = 2

val list=List(1, 1, 2, 3, 5, 8)

def findK(k:Int,l:List[Int], ind:Int=0):Option[Int] =  l match {
  case x::xs if ind!=k => findK(k,xs,ind+1)
  case x::xs if ind==k => Some(x)
  case _ => None
}

findK(5,list).getOrElse(None)

//better way
def findN[T](n:Int,l:List[T]):T= (n, l) match{
  case (0,x::xs)=> x
  case (n,x::xs)=> findN(n-1,xs)
  case (_,Nil) => throw new RuntimeException("No such element")
}
findN[Int](3,list)