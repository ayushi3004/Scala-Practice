//returns the largest element in a list of integers.
val l= List(1,4,2,-3,34,5,12,8,2,45,12,0,50,2)

def findMax(l: List[Int], m:Int = Integer.MIN_VALUE):Int= l match{
  case x::xs if x >= m => findMax(xs,x)
  case x::xs => findMax(xs,m)
  case Nil=>m
}

findMax(l)