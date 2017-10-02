//Modify the result of problem P10 in such a way that if an element has no
// duplicates it is simply copied into the result list. Only elements with
// duplicates are transferred as (N, E) terms.
//Example:
//  scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

//  val l = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
//  def encodeList(list: List[Symbol], inTup: Option[(Int, Symbol)] = None, outList: List[Any] = Nil): List[Any] = list match {
//    case x :: xs => inTup match {
//      case Some(inTup) => x match {
//        case inTup._2 => encodeList(xs, Some(inTup._1 + 1, inTup._2), outList)
//        case _ =>
//          if (inTup._1 == 1)
//            encodeList(xs, Some(1, x), outList :+ Some(inTup._2))
//          else
//            encodeList(xs, Some(1, x), outList :+ Some(inTup))
//      }
//
//      case None => encodeList(xs, Some(1, x), outList)
//    }
//
//    case Nil => inTup match {
//      case Some(inTup) =>
//        if (inTup._1 == 1)
//          outList :+ Some(inTup._2)
//        else
//          outList :+ Some(inTup)
//
//      case None => outList
//    }
//  }
//
//  encodeList(l).collect { case Some(e) => e }
//  encodeList(List('a, 'f, 'a)).collect { case Some(e) => e }
//  encodeList(List()).collect { case Some(e) => e }

val l = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

l.map{ e => if(e._1==1) e._2
            else e
}
