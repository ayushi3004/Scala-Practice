//English number words.
//  On financial documents, like checks, numbers must sometimes
//be written in full words. Example: 175 must be written as one-seven-five.
//  Write a function fullWords(num: Int) to print (non-negative)
//integer numbers in full words.

val words= List("zero","one","two","three","four","five","six","seven","eight","nine")

val numToWords =(num:Int) => {
  val str=num.toString

  for(c<-str) print(words(c.asDigit)+'-')
}
numToWords(175)
