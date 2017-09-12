package com.indix.bootcamp.scrabble

class Scrabble {
  val scoreMap = Map('A'-> 1, 'B' -> 3 , 'C' -> 3, 'D' -> 2 , 'E' -> 1 , 'F' -> 4,
    'G'-> 2, 'H' -> 4 , 'I' -> 1, 'J' -> 8 , 'K' -> 5 , 'L' -> 1,
    'M'-> 3, 'N' -> 1 , 'O' -> 1, 'P' -> 3 , 'Q' -> 10, 'R' -> 1,
    'S'-> 1, 'T' -> 1 , 'U' -> 1, 'V' -> 4 , 'W' -> 4 , 'X' -> 8,
    'Y'-> 4, 'Z' -> 10 )
  val twList = List((0, 0), (7, 0), (14, 0), (0, 7), (14, 7), (0, 14), (7, 14), (14, 14))
  val dwList = List((1, 1), (2, 2), (3, 3), (4, 4), (7, 7), (10, 4), (11, 3), (12, 2), (13, 1))
  val dlList = List((3, 0), (11, 0), (6, 2), (7, 3), (8, 2), (0, 3), (14, 3), (2, 6), (3, 7), (11, 7), (12, 6), (2, 8), (6, 8), (8, 8), (12, 8), (0, 11), (7, 11), (14, 11), (6, 12), (8, 12), (3, 14), (11, 14))
  val tlList = List((5, 1), (9, 1), (1, 5), (5, 5), (9, 5), (13, 5), (1, 9), (5, 9), (9, 9), (13, 9), (5, 13), (9, 13))


  def getInput(): (Int, Int, Char, String) = {
    val input = scala.io.StdIn
    val position = input.readLine().split(" ")
    val x = Integer.parseInt(position(0))
    val y = Integer.parseInt(position(1))
    val dir = position(2).charAt(0)

    val word = input.readLine()

    (x, y, dir, word)
  }

  def assignPoints(board: Array[Array[(String, Char)]], x: Int, y: Int, c: Char): Array[Array[(String, Char)]] = {
    val twListContains: Boolean = twList.contains((x, y))
    val dwListContains: Boolean = dwList.contains((x, y))
    val dlListContains: Boolean = dlList.contains((x, y))
    val tlListContains: Boolean = tlList.contains((x, y))

    if (!twListContains & !dwListContains & !dlListContains & !tlListContains) {
      board(x)(y) = (" ", c.toUpper)
    }

    else {
      if (twListContains)
        board(x)(y) = ("TW", c.toUpper)
      else if (dwListContains)
        board(x)(y) = ("DW", c.toUpper)
      else if (dlListContains)
        board(x)(y) = ("DL", c.toUpper)
      else if (tlListContains)
        board(x)(y) = ("TL", c.toUpper)
    }
    board
  }

  def setWord(wordInfo: (Int, Int, Char, String),board: Array[Array[(String, Char)]]=Array.ofDim[(String, Char)](15, 15)): Array[Array[(String, Char)]] = {
    val x = wordInfo._1
    val y = wordInfo._2
    val dir = wordInfo._3
    val word = wordInfo._4


    if (dir.toUpper == 'R') {

      if(x+word.length > 15)
        throw new ExceedBoardSizeException("Word exceeds board size")

      //misc=(board,x)
      //c each char of word
      val (finalBoard, _) = word.foldLeft((board, x)) {
        (misc, c) => {
          val b = misc._1
          val xCoord = misc._2
          (assignPoints(b, xCoord, y, c), xCoord + 1)
        }
      }
      finalBoard
    }

    else if(dir.toUpper == 'D'){
      if((y+1)-word.length < 0)
        throw new ExceedBoardSizeException("Word exceeds board size")

      val (finalBoard, _) = word.foldLeft((board, y)) {
        (misc, c) => {
          val b = misc._1
          val yCoord = misc._2
          (assignPoints(b, x, yCoord, c), yCoord - 1)
        }
      }
      finalBoard
    }
    else
      throw new WrongDirectionException("Direction can only be 'R' or 'D'")
  }

  def calcScore(board:Array[Array[(String, Char)]], wordInfo: (Int,Int,Char,String),lcount:Int=0,score:Int=0, dw:Int=0, tw:Int=0) : (Int,Int,Int)={
    val x = wordInfo._1
    val y = wordInfo._2
    val dir = wordInfo._3
    val word = wordInfo._4

    if(lcount<word.length) {
      if (dir.toUpper == 'R') {
        board(x)(y)._1 match {
          case "DL" => {
            val lscore: Int = scoreMap(board(x)(y)._2) * 2
            calcScore(board, (x + 1, y, 'R', word), lcount + 1, score + lscore, dw, tw)

          }

          case "TL" => {
            val lscore: Int = scoreMap(board(x)(y)._2) * 3
            calcScore(board, (x + 1, y, 'R', word), lcount + 1, score + lscore, dw, tw)
          }

          case "DW" => {
            val lscore: Int = scoreMap(board(x)(y)._2)
            calcScore(board, (x + 1, y, 'R', word), lcount + 1, score + lscore, dw + 1, tw)
          }

          case "TW" => {
            val lscore: Int = scoreMap(board(x)(y)._2)
            calcScore(board, (x + 1, y, 'R', word), lcount + 1, score + lscore, dw, tw + 1)
          }

          case " " => {
            val lscore: Int = scoreMap(board(x)(y)._2)
            calcScore(board, (x + 1, y, 'R', word), lcount + 1, score + lscore, dw, tw)
          }
        }
      }

      else if(dir.toUpper == 'D'){
        board(x)(y)._1 match {
          case "DL" => {
            val lscore: Int = scoreMap(board(x)(y)._2) * 2
            calcScore(board, (x , y-1, 'D', word), lcount + 1, score + lscore, dw, tw)

          }

          case "TL" => {
            val lscore: Int = scoreMap(board(x)(y)._2) * 3
            calcScore(board, (x , y-1, 'D', word), lcount + 1, score + lscore, dw, tw)
          }

          case "DW" => {
            val lscore: Int = scoreMap(board(x)(y)._2)
            calcScore(board, (x , y-1, 'D', word), lcount + 1, score + lscore, dw + 1, tw)
          }

          case "TW" => {
            val lscore: Int = scoreMap(board(x)(y)._2)
            calcScore(board, (x , y-1, 'D', word), lcount + 1, score + lscore, dw, tw + 1)
          }

          case " " => {
            val lscore: Int = scoreMap(board(x)(y)._2)
            calcScore(board, (x , y-1, 'D', word), lcount + 1, score + lscore, dw, tw)
          }
        }
      }
      else
       throw new WrongDirectionException("Direction can only be 'R' or 'D'")
    }
    else
      (score,dw,tw)
  }

  def getScore(board:Array[Array[(String, Char)]], wordInfo: (Int,Int,Char,String)):Int={
    val (score,dw,tw)=calcScore(board,wordInfo)
    if(dw == 0){
      if(tw==0){
        score
      }
      else
        score*Math.pow(3,tw).toInt
    }
    else
      score*Math.pow(2,dw).toInt
  }
}

object Main extends App{
  val input = scala.io.StdIn
  val scrabble = new Scrabble

  val testNum = input.readInt()
  for(i<- 0 until testNum){
    val wordInfo : (Int,Int,Char,String) = scrabble.getInput()
    val finalBoard = scrabble.setWord(wordInfo)
    val score=scrabble.getScore(finalBoard,wordInfo)
    println(score)
  }
}

class WrongDirectionException(str:String) extends Exception(str){
}

class ExceedBoardSizeException(str:String) extends Exception(str){
}
