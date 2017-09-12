package bootcamp.barter

class Barter {

  def parseInput(str:String,barterMap: Map[String, Double]): Map[String, Double] = {
    println(barterMap)
    val arr=str.split(" ")
    val q1 = arr(1)
    val i1 = arr(2).toLowerCase
    val q2 = arr(4)
    val i2 = arr(5).toLowerCase()

    if (barterMap.contains(i1) || barterMap.contains(i2)) {
      if (barterMap.contains(i1)) {
        val a = q1.toDouble / q2.toDouble * barterMap(i1)
        barterMap + ((i2, a))
      }
      else {
        val a = q2.toDouble / q1.toDouble * barterMap(i2)
        barterMap + ((i1, a))
      }
    }
    else {
      val a = q1.toDouble / q2.toDouble
      barterMap + ((i1, 1.toDouble)) + ((i2, a))
    }
  }

  @throws(classOf[InvalidQueryException])
  def findRatio(i1:String,i2:String,barterMap: Map[String, Double]): Tuple2[Double,Double] = {
    val a = barterMap.getOrElse(i1, throw new InvalidQueryException("Items not present in system"))
    val b = barterMap(i2)
    val a1:Double = 1.toDouble / a.toDouble
    val a2:Double = 1.toDouble / b.toDouble
//    val res: Double = Math.floor((a1 * a2))
//
//    val res1: Int = (Math.floor(a1 * res)).toInt
//    val res2: Int = (Math.floor(a2 * res)).toInt
//    (res1,res2)
    (a1,a2)
  }

  def inputToMap(assertionNum:Int,list:List[String]=List[String]()): List[String]={
    if(assertionNum>0) {
      val input = scala.io.StdIn
      val assertion = input.readLine()
      inputToMap(assertionNum-1, list :+ assertion)
    }
    else
      list
  }
}

object Main extends App {
  val input = scala.io.StdIn
  val barter = new Barter

  val t = input.readInt()
  val assertionNum = input.readInt()
  val queriesNum = input.readInt()

  val list=barter.inputToMap(assertionNum)

  val finMap=list.foldLeft(Map[String,Double]()){(m,i)=>{
    barter.parseInput(i,m)
  }}


  println("Enter query")
  for(i<- 0 until queriesNum){
    val input = scala.io.StdIn
    val query = input.readLine()
    val arr = query.split(" ")

    val i1 = arr(1)
    val i2 = arr(3)
    val tup=barter.findRatio(i1.toLowerCase(),i2.toLowerCase(),finMap)
    println(tup._1+" "+i1+" = "+tup._2+" "+i2)
  }
}


class InvalidQueryException(exp:String) extends Exception(exp){
}