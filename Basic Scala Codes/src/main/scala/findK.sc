//Find the Kth element of a list.
//  By convention, the first element in the list is element 0.
//Example:
//
//  scala> nth(2, List(1, 1, 2, 3, 5, 8))
//res0: Int = 2

val list=List(1, 1, 2, 3, 5, 8)

val findK: (Int,List[Int], Int)=> Int = (k,l,ind) => l match {
  case x::xs if ind!=k => findK(k,xs,ind+1)
  case x::xs if ind==k => x
  case _ =>
}

val findEle=(n: Int, l: List[Int])=>{
  findK(n,l,0)
}

findEle(3,list)


//better way
def findN[T](n:Int,l:List[T]):T= (n, l) match{
  case (0,x::xs)=> x
  case (n,x::xs)=> findN(n-1,xs)
  case (_,Nil) => throw new RuntimeException("No such element")
}
findN[Int](3,list)