//Find out whether a list is a palindrome.
//Example:
//  scala> isPalindrome(List(1, 2, 3, 2, 1))
//res0: Boolean = true

val l= List(1,2,3,3,2,1)


def isPalindrome(l: List[Int], s: Int = 0, e: Int = l.size - 1): Boolean = {
  if (s <= e) {
    if (l(s) == l(e)){
      isPalindrome(l, s + 1, e - 1)
      return true
    }
    else return false
  }
  true
}

isPalindrome(l)
