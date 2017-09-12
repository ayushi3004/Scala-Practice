package com.indix.bootcamp.marsrover

import org.scalatest.FlatSpec

class MarsRoverTest extends FlatSpec {
  "MarsRover" should "throw Exception if position exceeds plateau size" in {

    intercept[ExceedPlateauException] {
      val plateau = Plateau(5, 5)
      plateau.validatePosition(9, 8)
    }
  }

    it should "move forward correctly" in {
      assertResult(MarsRover(2, 3, 'N')) {
        val rover1 = MarsRover(2, 2, 'N')
        MarsRover.trackMovement("M", rover1)
      }
    }


    it should "turn left correctly" in{
      assertResult(MarsRover(2,5,'W')){
        val rover1=MarsRover(2,5,'N')
        MarsRover.trackMovement("L", rover1)
      }
    }

    it should "turn right correctly" in{
      assertResult(MarsRover(2,5,'W')){
        val rover1=MarsRover(2,5,'S')
        MarsRover.trackMovement("R", rover1)
      }
    }

    it should "accept and perform string of instruction" in {
      assertResult(MarsRover(5,1,'E')){
        val rover1=MarsRover(3,3,'E')
        MarsRover.trackMovement("MMRMMRMRRM", rover1)
      }
    }

    it should "throw WrongInstructionException if instruction is except M,L,R" in{
      intercept[WrongInstructionException]{
        val rover1=MarsRover(3,3,'E')
        MarsRover.trackMovement("AMRDMGMNRM", rover1)
      }
    }
}
