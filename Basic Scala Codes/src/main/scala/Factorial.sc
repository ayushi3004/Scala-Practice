//def factorial(num : Int) : Int={
//  if(num>0) num*factorial(num-1)
//  else 1
//
//}
//factorial(4)

//TAIL RECURSION
def factorial2(num:Int, fact:Int=1):Int={
  if(num>0) factorial2(num-1,fact*num)
  else fact
}

factorial2(5)