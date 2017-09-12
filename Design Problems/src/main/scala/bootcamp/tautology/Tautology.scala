package com.indix.bootcamp.tautology

class Tautology {

//
//  def createUniformExp(list:List[Char]):List[Char]=list match{
//
//
//  }

  def isAlphabet(name:String):Boolean={
    name.matches("[a-zA-Z]")
  }

  def popUntilStart(expList:List[Char],postfixList: List[Char], st: List[Char]):(List[Char],List[Char])={
    if(st.head!='(')
      popUntilStart(expList,postfixList:+st.head,st.tail)
    else
      (postfixList,st)
  }

  def postfix(expList:List[Char], postfixList: List[Char]=List[Char](),st:List[Char]=List[Char](),set:Set[Char]=Set[Char]()):(List[Char],Set[Char])= expList match {
    case x::xs if x=='('  | x == '|'| x=='&' | x=='!'=> postfix(xs,postfixList,x::st,set)
    case x::xs if isAlphabet(x.toString) => postfix(xs,postfixList:+x,st,set+x)
    case x::xs if x==')' => {
      val (p,s)=popUntilStart(expList,postfixList,st)
      postfix(xs,p,s,set)
    }
    case Nil => (postfixList,set)
  }

  def createTT(postfixList:List[Char],set:Set[Char], binaryString:String=""):Boolean={
    val fin=(0 until Math.pow(2,set.size).toInt).toList.foldLeft(true){ (ans,i) =>
      val binaryString = String.format("%"+Integer.toString(set.size)+"s",Integer.toBinaryString(i)).replace(" ","0")
      val md=Map[Char,Boolean]()
      val misc =set.foldLeft( (0,md) ){
        (m,c)=> {
          val a = m._1
          val temp={
            if(Integer.parseInt(binaryString.charAt(a).toString)==0)
              false
            else
              true
          }
          val b = m._2 + ((c,temp))
          (a+1,b)
        }
      }
      val map = misc._2
      for((k,v)<-map){
        println(k+" "+v)
      }
      val res:Boolean=solveExp(map,postfixList)
      val send=ans&res
      println(send)
      send
    }
    fin
  }

  def solveExp(map:Map[Char,Boolean],postfixList: List[Char]):Boolean={
    val ans=postfixList.foldLeft( (false,false,true)){
      (op, ele)=> {
        val op1:Boolean  = op._1
        val op2:Boolean = op._2
        val firstEle:Boolean = op._3

        if(isAlphabet(ele.toString)){
          if(firstEle==true)
            (map(ele),map(ele),false)
          else
            (op2,map(ele),false)
        }

        else if(ele == '!'){
          (op1,!op2,false)
        }
        else if(ele == '&'){
          (op1 & op2,false,false)
        }
        else
          (op1 | op2, false,false)
      }
    }
    (ans._1)
  }
}

object Main extends App{
  val input=scala.io.StdIn
  val inputExp=input.readLine().replaceAll("\\s+", "").toList
  val obj = new Tautology
  //val exp=obj.createUniformExp(inputExp)
  val (postfix,varSet)=obj.postfix(inputExp)
  println(postfix)
//  val result:Boolean=obj.createTT(postfix,varSet)
//  println("-----------------")
//  println(result)
}
