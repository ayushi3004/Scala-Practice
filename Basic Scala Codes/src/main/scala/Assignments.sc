//print elements in the list using recursion
val l=(1 to 10).toList

def printList(l: List[Int]): Unit =l match{

  case x::xs =>
    println(x)
    printList(xs)
  case _ =>
}

printList(l)

/////////////////////////////////////

//sum of all elements of list using recursion

def newSum(l: List[Int], s: Int = 0): Int = l match {
  case Nil => s
  case x :: xs => newSum(xs, s + x)
}

def foldSum(l: List[Int]) = l.fold(0) { (i: Int, i0: Int) => {
  i +i0
} }


l.fold(0){_+_}

newSum(l)
foldSum(l)

////////////////////////////////////
//returns the largest element in a list of integers.
val l1= List(1,4,2,-3,34,5,12,8,2,45,12,0,50,2)

def findMax(l: List[Int], m:Int = Integer.MIN_VALUE):Int= l match{
  case x::xs if x >= m => findMax(xs,x)
  case _::xs => findMax(xs,m)
  case Nil=>m
}

findMax(l1)
///////////////////////////////////


//////////////////////////////////////

//find last element of list

def findLast(l : List[Int]):Int = l match {
  case Nil => throw new RuntimeException("Cant find last in empty")
  case x :: Nil => x
  case _::xs => findLast(xs)
}

findLast(l)

/////////////////////////////////////
//find penultimate element of a list

def findPen(l:List[Int]) : Int = l match{
  case Nil => throw new RuntimeException("can't find penultimate element")
  case x:: _ ::Nil => x
  case _::xs => findPen(xs)
}

findPen(l)

////////////////////////////////////
//reverse a list


//using func

def reverse(l: List[Int]) : Unit=l match{
  case x::xs =>
    reverse(xs)
    println(x)
  case _ =>
}
reverse(l)

//using fold

l.foldLeft(List.empty[Int]){ (b, a) => a :: b }

/////////////////////////////////
def factorial2(fact:Int, num:Int):Int={
  if(num>0) factorial2(fact*num,num-1)
  else fact
}

factorial2(1,5)

///////////////////////////////

//Drop every Nth element from a list.
//  scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
val l2 =  List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
val drop2 : (Int, List[Symbol], Int) => List[Symbol] ={(num, l, ind)=> l match {
  case x::xs if ind%num==0 => drop2(num,xs,ind+1)
  case x::xs if ind%num!=0 => x::drop2(num,xs,ind+1)
  case Nil => Nil
  }
}

////////////////////////////////////

//Duplicate the elements of a list a given
// number of times.
//scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
//res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)

val l3= List('a, 'b, 'c, 'c, 'd)

val duplicateN:(Int,List[Symbol])=>List[Symbol]=
  (num,l)=> l.flatMap(List.fill(num)(_))

duplicateN(3,l3)

///////////////////////////////////////

//Eliminate consecutive duplicates of list
// elements.
//If a list contains repeated elements they
// should be replaced with a single copy of
// the element. The order of the elements should
// not be changed.
// scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

val l4= List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

def dedup[Int](l: List[Int]):List[Int] = l match {
  case Nil => Nil
  case x::List() => List(x)
  case x::xs if (x == xs.head) => dedup(xs)
  case x::xs => x::dedup(xs)
}
dedup(l4)

//////////////////////////////

//Extract a slice from a list.
//Given two indices, I and K, the slice is the list containing the elements from and
//  including the Ith element up to but not including the Kth element of the original list.
//Start counting the elements with 0.
//scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: List[Symbol] = List('d, 'e, 'f, 'g)

val l5= List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

def slice(s:Int, e:Int, l:List[Symbol]): List[Symbol] ={
  l.filter( (i:Symbol) => (
    l.indexOf(i) >=s && l.indexOf(i)<e)
  )
}
slice(3,7,l5)

///////////////////////////////

//Multiples of 3 and 5
//If we list all the natural numbers below 10 that are
//multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these
//multiples is 23. Find the sum of all the multiples
//  of 3 or 5 below 1000.

val multiple3n5={
  for(i<-3 until 1000 if(i%3==0 || i%5==0)) yield i
}

multiple3n5.reduceLeft(_+_)

/////////////////////////////

//English number words.
//  On financial documents, like checks, numbers must sometimes
//be written in full words. Example: 175 must be written as one-seven-five.
//  Write a function fullWords(num: Int) to print (non-negative)
//integer numbers in full words.

val words= List("zero","one","two","three","four","five","six","seven","eight","nine")

val numToWords =(num:Int) => {
  val str=num.toString

  for(c<-str) print(words(c.asDigit)+'-')
}
numToWords(175)

//////////////////////////////
// EDIT-2









//Find the Kth element of a list.
//  By convention, the first element in the list is element 0.
//Example:
//
//  scala> nth(2, List(1, 1, 2, 3, 5, 8))
//res0: Int = 2
val l6=List(1, 1, 2, 3, 5, 8)

def findN[T](n:Int,l:List[T]):T= (n, l) match{
  case (0,x::xs)=> x
  case (n,x::xs)=> findN(n-1,xs)
  case (_,Nil) => throw new RuntimeException("No such element")
}
findN[Int](3,l6)
//////////////////////////////

//Find the number of elements of a list.
//Example:
//  scala> length(List(1, 1, 2, 3, 5, 8))
//res0: Int = 6

val l7= List(1, 1, 2, 3, 5, 8)

def findLength[T](l:List[T],cnt:Int=0):Int = l match{
  case x::xs => findLength(xs,cnt+1)
  case Nil => cnt
}

findLength(l7)

///////////////////////////////
//Find out whether a list is a palindrome.
//Example:
//  scala> isPalindrome(List(1, 2, 3, 2, 1))
//res0: Boolean = true

val l8= List(1,2,3,3,2,1)
val s=0
val e=0

def isPalindrome[T](l: List[T], s: Int = 0, e: Int = l.size - 1): Boolean = {
  if (s <= e) {
    if (l(s) == l(e)){
      isPalindrome(l, s + 1, e - 1)
      true
    }
    else false
  }
  true
}

isPalindrome(l8)

////////////////////////////////////
//Flatten a nested list structure.
//Example:
//  scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
//res0: List[Any] = List(1, 1, 2, 3, 5, 8)

val l9=List(List(1, 1), 2, List(3, List(5, 8)))

def flatten(l:List[Any]):List[Any]={
  l.flatMap(_ match {
    case l1:List[Any] => flatten(l1)
    case s:Int => List(s)
  })
}
flatten(l9)


////////////////////////////////
//Split a list into two parts.
//The length of the first part is given. Use a Tuple for your result.
//Example:
//
//  scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

val l10= List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

//Tuple2[List[Symbol], List[Symbol]]

def splitList(num:Int, l:List[Symbol], ind: Int=0, res:(List[Symbol],List[Symbol])=(Nil,Nil)) : (List[Symbol], List[Symbol]) = l match{
  case x::xs if ind<num => splitList(num,xs,ind+1,(res._1:+x,res._2))
  case x::xs if ind>=num => splitList(num,xs,ind+1,(res._1,res._2:+x))
  case Nil => res
}


splitList(3,l10)


///////////////////////////////
//Pack consecutive duplicates of list elements into sublists.
//If a list contains repeated elements they should be placed in separate sublists.
//  Example:
//
//  scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

val l11:List[Symbol]=List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

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

packCons(l11)
packCons(List('x,'a))




