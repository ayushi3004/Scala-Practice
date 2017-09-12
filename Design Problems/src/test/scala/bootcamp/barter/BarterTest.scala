package com.indix.bootcamp.barter

import bootcamp.barter.Barter
import bootcamp.barter.InvalidQueryException
import org.scalatest.FlatSpec

class BarterTest extends FlatSpec{
  val finMap= Map("cow" -> 0.6000000000000001, "bulb" -> 0.5, "tv" -> 1.0, "ball" -> 0.375, "box" -> 0.4)
  "Barter system" should "return correct result" in{
    assertResult((2.5,2.0)){
      val barter= new Barter
      barter.findRatio("box","bulb",finMap)
    }
  }

  it should "be case insensitive" in{
    assertResult((2.5,2.6666666666666665)){
      val barter= new Barter
      barter.findRatio("Box","BuLb",finMap)
    }
  }

  it should " throw exception if items not defined in barter system" in{
    intercept[InvalidQueryException]{
      val barter= new Barter
      barter.findRatio("Tubelight ","BuLb",finMap)
    }
  }
}
