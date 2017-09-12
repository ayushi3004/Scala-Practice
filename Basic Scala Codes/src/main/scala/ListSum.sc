//sum of all elements of list using recursion
val l=(1 to 10).toList

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