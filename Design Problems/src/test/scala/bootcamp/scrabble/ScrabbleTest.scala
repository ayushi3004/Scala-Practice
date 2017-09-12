package com.indix.bootcamp.scrabble

import org.scalatest.FlatSpec

class ScrabbleTest extends FlatSpec {
  val scrabble = new Scrabble
  "Scrabble" should "return correct score" in {
    assertResult(34){
      val wordInfo=(7,8,'D',"bootcamp")
      val finalBoard = scrabble.setWord(wordInfo)
      scrabble.getScore(finalBoard,wordInfo)
    }
  }

  it should "be case insensitive" in {
    assertResult(999){
      val wordInfo=(0,0,'r',"cryptoZOOLogies")
      val finalBoard = scrabble.setWord(wordInfo)
      scrabble.getScore(finalBoard,wordInfo)
    }
  }

  it should "throw an exception when wrong direction is entered" in{
    intercept[WrongDirectionException]{
      val wordInfo=(0,0,'L',"indix")
      val finalBoard = scrabble.setWord(wordInfo)
      scrabble.getScore(finalBoard,wordInfo)
    }
  }

  it should "throw an exception when the word to be places exceeds board size(right)" in{
    intercept[ExceedBoardSizeException]{
      val wordInfo=(14,0,'R',"indix")
      val finalBoard = scrabble.setWord(wordInfo)
      scrabble.getScore(finalBoard,wordInfo)
    }
  }

  it should "throw an exception when the word to be places exceeds board size(down)" in{
    intercept[ExceedBoardSizeException]{
      val wordInfo=(1,0,'D',"indix")
      val finalBoard = scrabble.setWord(wordInfo)
      scrabble.getScore(finalBoard,wordInfo)
    }
  }
}
