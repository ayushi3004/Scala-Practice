//Run-length encoding of a list.
//  Use the result of problem P09 to implement the so-called run-length
// encoding data compression method. Consecutive duplicates of elements are encoded as
// tuples (N, E) where N is the number of duplicates of the element E.
//Example:
//  scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

  val l = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

  def encodeList(list: List[Symbol], inTup: Option[(Int, Symbol)] = None, outList: List[Option[(Int, Symbol)]] = Nil): List[(Int, Symbol)] = list match {
    case x :: xs => inTup match {
      case Some(inTup) => x match {
        case inTup._2 => encodeList(xs, Some(inTup._1 + 1, inTup._2), outList)
        case _ => encodeList(xs, Some(1, x), outList :+ Some(inTup))
      }

      case None => encodeList(xs, Some(1, x), outList)
    }

    case Nil => (outList :+ inTup).flatten
  }

  encodeList(l)
  encodeList(List('a, 'f, 'a))
  encodeList(List())
