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
def factorial2(num:Int, fact:Int=1):Int={
  if(num>0) factorial2(num-1,fact*num)
  else fact
}

factorial2(5)

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

def drop3(num:Int, l:List[Symbol]): List[Symbol] ={
  l.foldLeft((List[Symbol](),1)){
    (a,z) =>
      if(a._2%num==0)
        (a._1,a._2+1)
      else
        (a._1:+z,a._2+1)
  }._1
  //or

  l.zipWithIndex.filter(e => (e._2+1)%num != 0).map(_._1)
}
drop3(3,l2)
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

// or
// or
def dedup2(l:List[Symbol]): List[Symbol] ={
  if(l.isEmpty)
    l
  else
    l.foldLeft(List[Symbol](l.head)){
      (a,z) =>
        if(z != a.head)
          z::a
        else
          a
    }.reverse
}

dedup2(l4)
dedup(List( ))

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

def slice2(s:Int, e:Int, li:List[Symbol], n: Int = 0, resList: List[Symbol] = List[Symbol]()): List[Symbol] = li match {
  case x::xs if n<s | n>=e    => slice2(s,e,xs,n+1,resList)
  case x::xs if n>=s && n<e   => slice2(s,e,xs,n+1, resList:+x)
  case Nil => resList
}
slice2(3,7,l5)


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
def findK(k:Int,l:List[Int], ind:Int=0):Option[Int] =  l match {
  case x::xs if ind!=k => findK(k,xs,ind+1)
  case x::xs if ind==k => Some(x)
  case _ => None
}

findK(5,l6).getOrElse(None)
//or
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

def isPalindrome[T](l: List[T], s: Int = 0, e: Int = l8.size-1): Boolean = {
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


///////////////////////////////
//Run-length encoding of a list.
//  Use the result of problem P09 to implement the so-called run-length
// encoding data compression method. Consecutive duplicates of elements are encoded as
// tuples (N, E) where N is the number of duplicates of the element E.
//Example:
//  scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

val l12= List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

def encodeList(list:List[Symbol], inTup:Option[(Int,Symbol)]=None, outList:List[Option[(Int,Symbol)]]= Nil): List[(Int,Symbol)]= list match {
  case x :: xs => inTup match {
    case Some(inTup) => x match{
      case inTup._2 => encodeList(xs, Some(inTup._1 + 1, inTup._2), outList)
      case _ => encodeList(xs, Some(1, x), outList :+ Some(inTup))
    }

    case None => encodeList(xs, Some(1, x), outList)
  }

  case Nil => (outList :+ inTup).flatten
}

encodeList(l12)
encodeList(List('a,'f,'a))
encodeList(List())

////////////////////////////////////////////
//Modify the result of problem P10 in such a way that if an element has no
// duplicates it is simply copied into the result list. Only elements with
// duplicates are transferred as (N, E) terms.
//Example:
//  scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))


encodeList(l12).map{ e => if(e._1==1) e._2
else e
}

////////////////////////////////////////
//Decode a run-length encoded list.
//  Given a run-length code list generated as specified in problem P10,
// construct its uncompressed version.
//Example:
//  scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
//res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

encodeList(l12).flatMap{e => List.fill(e._1)(e._2)}

/////////////////////////////////////
//Rotate a list N places to the left.
//Examples:
//  scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
//
//scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)

val l13 =  List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

def rotate(places:Int, li: List[Symbol]) : List[Symbol] = {
  val n = if(li.isEmpty) 0 else places%li.size

  if(places < 0 )
    rotate(n + li.size, li)
  else
    li.drop(n) ::: li.take(n)
}

rotate(3, l13)

//Remove the Kth element from a list.
//  Return the list and the removed element in a Tuple. Elements are numbered from 0.
//Example:
//
//  scala> removeAt(1, List('a, 'b, 'c, 'd))
//res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)

val l14 = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

def removeK(k:Int, li:List[Symbol]): (List[Symbol], Symbol) = {
  (li.take(k) ::: li.drop(k+1), li.zipWithIndex.find(_._2==k).get._1)
}

//OR

def removeK2(k:Int, li:List[Symbol]): (List[Symbol], Symbol) = (k,li) match{
  case (_, Nil) => throw new NoSuchElementException
  case (0, x::xs) => (xs, x)
  case (_,x::xs) => {
    val (resultList, removedEle) = removeK2(k-1, xs)
    (x::resultList, removedEle)
  }
}

removeK(5,l14)
removeK2(3,l14)





