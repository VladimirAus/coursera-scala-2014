package week01

object lecture {
  // sbt console to run scala console
  def square(x: Double) = x*x                     //> square: (x: Double)Double
  //square
  square(2)                                       //> res0: Double = 4.0
  square(5+4)                                     //> res1: Double = 81.0
  square(square(4))                               //> res2: Double = 256.0
  
  def sumOfSquares(x: Double, y: Double) = square(x) + square(y)
                                                  //> sumOfSquares: (x: Double, y: Double)Double

	def isEqual(x: Int, y: Double):Boolean = x == y
                                                  //> isEqual: (x: Int, y: Double)Boolean
	def loop: Int = loop                      //> loop: => Int

	// termination on Call-By-Name, but not Call-By-Value
	def first(x:Int, y:Int) = x               //> first: (x: Int, y: Int)Int
	// first(1, loop)
	// Call-By-Value is imperative choice, but to force CBN
	def constOne(x:Int, y: => Int) = 1        //> constOne: (x: Int, y: => Int)Int
	constOne(1, loop)                         //> res3: Int = 1
	
	// if-else
	def abs(x: Double) = if (x >= 0) x else -x//> abs: (x: Double)Double
	
	// definitions
	def x = loop                              //> x: => Int
	// val y = loop // doen't work with val
	
	def and(x:Boolean, y:Boolean):Boolean  = if (x) y else false
                                                  //> and: (x: Boolean, y: Boolean)Boolean
	def or(x:Boolean, y:Boolean):Boolean  = if (!x) y else true
                                                  //> or: (x: Boolean, y: Boolean)Boolean
	and(true, true)                           //> res4: Boolean = true
	and(false, true)                          //> res5: Boolean = false
	and(true, false)                          //> res6: Boolean = false
	and(false, false)                         //> res7: Boolean = false
	or(true, true)                            //> res8: Boolean = true
	or(false, true)                           //> res9: Boolean = true
	or(true, false)                           //> res10: Boolean = true
	or(false, false)                          //> res11: Boolean = false
	
	// Sqrt implementation using a block and nesting
	def sqrt(x: Double): Double = {
		def sqrtIter(guess: Double): Double =
			if (isGoodEnough(guess)) guess
			else sqrtIter(improve(guess))
		def isGoodEnough(guess: Double): Boolean =
			abs(guess*guess-x)/x < 1e-13
		def improve(guess: Double): Double =
			(guess + x / guess) / 2
		
		sqrtIter(1.0)
	 }                                        //> sqrt: (x: Double)Double
	
	sqrt(2.0)                                 //> res12: Double = 1.414213562373095
  sqrt(4.0)                                       //> res13: Double = 2.000000000000002
  sqrt(1e-6)                                      //> res14: Double = 0.0010000000000000117
  sqrt(1e60)                                      //> res15: Double = 1.0E30
  
  sqrt(0.001)                                     //> res16: Double = 0.03162277660168433
  sqrt(0.1e-20)                                   //> res17: Double = 3.162277660168384E-11
  sqrt(1.0e20)                                    //> res18: Double = 1.0E10
  sqrt(1.0e50)                                    //> res19: Double = 1.0E25
  
  // Tail recursion
  // Great common devider: Euclid's algorythm
  def gcd(a: Int, b: Int): Int =
  	if (b == 0) a else gcd(b, a%b)            //> gcd: (a: Int, b: Int)Int
  	
  gcd(10,35)                                      //> res20: Int = 5
  
  def factorial(n: Int): Int =
  	if (n == 0) 1 else n * factorial(n-1)     //> factorial: (n: Int)Int
      
  factorial(4)                                    //> res21: Int = 24
  
  def factorialTL(n: Int): Int = {
  	def loop(acc: Int, n:Int):Int =
  		if (n == 0) acc
  		else loop(acc * n, n-1)
  	loop(1, n)
  }                                               //> factorialTL: (n: Int)Int
      
  factorialTL(4)                                  //> res22: Int = 24
}