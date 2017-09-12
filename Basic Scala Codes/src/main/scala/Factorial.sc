//def factorial(num : Int) : Int={
//  if(num>0) num*factorial(num-1)
//  else 1
//
//}
//factorial(4)

//TAIL RECURSION
def factorial2(fact:Int, num:Int):Int={
  if(num>0) factorial2(fact*num,num-1)
  else fact
}

factorial2(1,5)