package com.indix.bootcamp.remotecontrol

import org.scalatest.FlatSpec

class RemoteControlTest extends FlatSpec {
  val remote = new RemoteControl
  "RemoteControl" should "give correct no. of clicks from going from 1 channel to another by pressing no. keys" in{
    assertResult(4) {
      remote.calcPressKeys((4, 100))
    }
  }

  it should "give correct result for clicks while while going up/down" in{
    assertResult(3){
      remote.calcUpDown((20,15), 1, 100, Array(16,17,21))
    }
  }

  it should "give correct result for clicks on pressing back" in{
    assertResult(1){
      remote.calcBack((100,78),(78,100), 1 , 100, Array())
    }
  }

  it should "give correct result for clicks on pressing back and combination of up/down" in{
    assertResult(2){
      remote.calcBack((100,78), (78,99), 1 , 100, Array())
    }
  }

  it should "find min of all clicks and return total min clicks" in{
    assertResult(12){
      remote.findTotalClicks(1,100,Array(78,79,80,3),Array(10,13,13,100,99,98,77,81))
    }
  }
}
