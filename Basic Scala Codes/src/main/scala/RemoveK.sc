//Remove the Kth element from a list.
//  Return the list and the removed element in a Tuple. Elements are numbered from 0.
//Example:
//
//  scala> removeAt(1, List('a, 'b, 'c, 'd))
//res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)

val l = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

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

//removeK(5,l)
removeK2(3,l)