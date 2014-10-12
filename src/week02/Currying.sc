package week02

object Currying {
	def id(x: Int) = x                        //> id: (x: Int)Int
	def square(x: Int) = x*x                  //> square: (x: Int)Int
	def cube(x: Int) = x*x*x                  //> cube: (x: Int)Int
	def sum(f: Int => Int)(a: Int, b: Int): Int =
		if (a > b) 0 else f(a) + sum(f)(a+1, b)
                                                  //> sum: (f: Int => Int)(a: Int, b: Int)Int
  
  def product(f: Int => Int)(a: Int, b: Int): Int =
  	if (a > b) 1 else f(a) * product(f)(a+1, b)
                                                  //> product: (f: Int => Int)(a: Int, b: Int)Int
  	
 	product(square)(3,4)                      //> res0: Int = 144
 	
 	def factorial(n: Int) = product(id)(1, n) //> factorial: (n: Int)Int
 	factorial(4)                              //> res1: Int = 24
 	
 	def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  	if (a > b) zero
  	else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
                                                  //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:
                                                  //|  Int)Int
  	
 	def rangeProducts(f: Int => Int)(a: Int, b: Int): Int =
 										mapReduce(f, (x, y) => x*y, 1)(a, b)
                                                  //> rangeProducts: (f: Int => Int)(a: Int, b: Int)Int
	rangeProducts(x => x*x)(3,4)              //> res2: Int = 144

}