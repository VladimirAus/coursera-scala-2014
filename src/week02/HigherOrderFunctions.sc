package week02

object HigherOrderFunctions {
  // First order function returns datatype
  // Higher order function returns function
  
  def id(a: Int): Int = a                         //> id: (a: Int)Int
  def sunInts(a: Int, b: Int): Int =
  	if (a > b) 0 else a + sunInts(a+1, b)     //> sunInts: (a: Int, b: Int)Int
  	
 	def cube(a: Int): Int = a*a*a             //> cube: (a: Int)Int
 	def sunCubs(a: Int, b: Int): Int =
 		if (a > b) 0 else cube(a) + sunCubs(a+1, b)
                                                  //> sunCubs: (a: Int, b: Int)Int
  def fact(n: Int): Int =
  	if (n == 0) 1 else n * fact(n-1)          //> fact: (n: Int)Int
  def sunFacts(a: Int, b: Int): Int =
 		if (a > b) 0 else fact(a) + sunFacts(a+1, b)
                                                  //> sunFacts: (a: Int, b: Int)Int
 		
 	// HOF
 	def sum(f: Int => Int, a: Int, b: Int): Int =
  	if (a > b) 0 else f(a) * sum(f, a+1, b)   //> sum: (f: Int => Int, a: Int, b: Int)Int
  	
  def sunIntsHof(a: Int, b: Int): Int = sum (id, a, b)
                                                  //> sunIntsHof: (a: Int, b: Int)Int
  def sunCubesHof(a: Int, b: Int): Int = sum (cube, a, b)
                                                  //> sunCubesHof: (a: Int, b: Int)Int
  def sunFactsHof(a: Int, b: Int): Int = sum (fact, a, b)
                                                  //> sunFactsHof: (a: Int, b: Int)Int
  
}