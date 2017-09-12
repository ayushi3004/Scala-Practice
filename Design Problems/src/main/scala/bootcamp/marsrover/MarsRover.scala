package com.indix.bootcamp.marsrover

case class Plateau(px:Int, py:Int){
  @throws(classOf[ExceedPlateauException])
  def validatePosition(x:Int, y:Int): Boolean ={
    if(x>px ||  y>py || x<0 || y<0) {
      throw new ExceedPlateauException("Exceeds/falls short plateau size")
    }
    else true
  }
}

case class MarsRover(x:Int,y:Int,orientation:Char='N'){

  def rotationChart(move:Char):MarsRover= {
    (orientation, move) match {
      case ('N', 'L') => this.copy(orientation='W')
      case ('N', 'R') => this.copy(orientation='E')
      case ('S', 'L') => this.copy(orientation='E')
      case ('S', 'R') => this.copy(orientation='W')
      case ('E', 'L') => this.copy(orientation='N')
      case ('E', 'R') => this.copy(orientation='S')
      case ('W', 'L') => this.copy(orientation='S')
      case ('W', 'R') => this.copy(orientation='N')
    }
  }

  def stepChart():MarsRover= {
      orientation match {
        case 'N' => this.copy(y=y+1)
        case 'S' => this.copy(y=y-1)
        case 'E' => this.copy(x=x+1)
        case 'W' => this.copy(x=x-1)
      }
  }

  def printPosition(): Unit ={
    println(x+" "+y+" "+orientation)
  }
}

object MarsRover {

  @throws(classOf[WrongInstructionException])
  def trackMovement(move: String, rover: MarsRover): MarsRover = {
    move.foldLeft(rover){ (r,c) => c match {
        case 'M' => r.stepChart()
        case 'L'| 'R' => r.rotationChart(c)
        case _ => throw new WrongInstructionException("Wrong Instruction")
      }
    }
  }
}

object Main extends App {
  val input = scala.io.StdIn

  val x = input.readInt()
  val y = input.readInt()

  val plateau = Plateau(x,y)

  var ans = ""
  while(!ans.equalsIgnoreCase("no")) {
    val x = input.readInt()
    val y = input.readInt()
    val o = input.readChar()
    plateau.validatePosition(x,y)

      val rover = MarsRover(x, y, o)
      val movement = input.readLine()
      val finRover = MarsRover.trackMovement(movement, rover)
      plateau.validatePosition(finRover.x,finRover.y)

      println(finRover)

      println("Continue?")
      ans = input.readLine()
  }
}

class ExceedPlateauException(exp:String) extends Exception(exp){
}

class WrongInstructionException(exp:String) extends Exception(exp){
}
