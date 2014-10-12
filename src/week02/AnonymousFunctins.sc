package week02

object AnonymousFunctins {
  (x: Int) => x*x*x                               //> res0: Int => Int = <function1>
  
  def sum(f: Int => Int, a: Int, b: Int): Int =
  	if (a > b) 0 else f(a) + sum(f, a+1, b)   //> sum: (f: Int => Int, a: Int, b: Int)Int
  	
  def sumTL(f: Int => Int)(a: Int, b: Int): Int = {
  	def loop(a: Int, acc:Int):Int =
  		if (a > b) acc
  		else loop(a + 1, acc + f(a))
  	loop(a, 0)
  }                                               //> sumTL: (f: Int => Int)(a: Int, b: Int)Int
  
  def sunIntsAf(a: Int, b: Int): Int = sumTL (x => x)(a, b)
                                                  //> sunIntsAf: (a: Int, b: Int)Int
  def sunCubesAf(a: Int, b: Int): Int = sumTL (x => x*x*x)(a, b)
                                                  //> sunCubesAf: (a: Int, b: Int)Int
     
  sunIntsAf(2,3)                                  //> res1: Int = 5
  sunCubesAf(2,3)                                 //> res2: Int = 35
}