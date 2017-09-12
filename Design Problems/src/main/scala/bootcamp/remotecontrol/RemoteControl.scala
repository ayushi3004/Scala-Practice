package com.indix.bootcamp.remotecontrol

class RemoteControl {
  //method to calculate min hits on keying in channel nos
  def calcPressKeys(currentChannelTuple: (Int,Int)):Int ={
    val pressed={
      if(currentChannelTuple._1==currentChannelTuple._2)
        0
      else {
        currentChannelTuple._2.toString.length
      }
    }
    pressed
  }

  //method to calculate clicks on pressing up/down
  def calcUpDown(currentChannelTuple: (Int, Int), lowest: Int, highest: Int, blockedChannels: Array[Int]):Int={
    val viewableList = (lowest to highest).toList.filter(!blockedChannels.contains(_))
    // if next channel is same as current channel
    if(currentChannelTuple._1==currentChannelTuple._2)
      0
    else {
      val p1: Int = viewableList.indexOf(currentChannelTuple._1)
      val p2: Int = viewableList.indexOf(currentChannelTuple._2)
      val upTemp = p2 - p1

      //min channel + series of up enters end channels
      val up = {
        if (upTemp < 0) {
          viewableList.size - p1 + p2
        }
        else {
          upTemp
        }
      }

      val p3: Int = viewableList.indexOf(currentChannelTuple._1)
      val p4: Int = viewableList.indexOf(currentChannelTuple._2)
      val downTemp = p3 - p4

      // max channel + series of down enters front channels
      val down = {
        if (downTemp < 0) {
          p3 + viewableList.size - p4
        }
        else {
          downTemp
        }
      }
      Math.min(up, down)
    }
  }

  //method to calculate clicks on pressing back or combination of back and up/down
  def calcBack(currentChannelTuple:(Int,Int), prevChannelTuple:(Int,Int), lowest:Int, highest:Int, blockedChannels:Array[Int]):Int = {
    // if next channel is same as current channel
    if(currentChannelTuple._1== currentChannelTuple._2)
      0
    else {
      //if just 1 back click would suffice
      if (currentChannelTuple == (prevChannelTuple._2,prevChannelTuple._1))
        1
      // if 1 back + up/down keys reqd.
      else{
        if(prevChannelTuple!=(0,0)) {
          val prevChannel = prevChannelTuple._1
          val clicks = calcUpDown((prevChannel, currentChannelTuple._2), lowest, highest, blockedChannels)
          clicks+1
        }
        else
          99999
      }
    }
  }

  //finds min of all 3 approaches
  def totalClicks(lowest:Int, highest:Int, blockedChannels:Array[Int], channelsToView :Array[Int], channelTupleList :List[(Int,Int)], total:Int, prevChannelTuple:(Int,Int)=(0,0)):Int=channelTupleList match{
    case x::xs => {
      val tot = total + Math.min(Math.min(calcPressKeys(x), calcUpDown(x, lowest, highest, blockedChannels)), calcBack(x, prevChannelTuple, lowest, highest, blockedChannels))
      totalClicks(lowest, highest, blockedChannels, channelsToView, xs, tot, x)
    }
    case _ => total
  }

  //caller method
  def findTotalClicks(lowest:Int, highest:Int, blockedChannels:Array[Int], channelsToView:Array[Int]):Int={
    val channelTupleList = channelsToView.sliding(2).map((ints: Array[Int]) => (ints(0), ints(1))).toList
    totalClicks(lowest, highest, blockedChannels,channelsToView,channelTupleList,channelTupleList.head._1.toString.length)
  }
}

object Main extends App{
  val input = scala.io.StdIn
  val remote= new RemoteControl
  val Array(lowest,highest) = input.readLine().split(" ").map(_.toInt)
  val blockedChannels:Array[Int] = input.readLine().split(" ").map(_.toInt).drop(1)
  val channelsToView:Array[Int] = input.readLine().split(" ").map(_.toInt).drop(1)
  val clicks= remote.findTotalClicks(lowest,highest,blockedChannels,channelsToView)
  println(clicks)
}
