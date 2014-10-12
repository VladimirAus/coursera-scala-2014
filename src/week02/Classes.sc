package week02

class Rational(x: Int, y: Int) {
	require(y != 0, "denominator must be non zero")
	
	// Constructor
	def this(x: Int) = this(x, 1)

	private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a%b)
 	val numer = x
  val denom = y
  
  def + (that: Rational): Rational =
		new Rational(
			numer * that.denom + denom * that.numer,
			denom * that.denom)
			
	override def toString = {
		val g = gcd(numer, denom)
		numer/g + "/" + denom/g
	}
	
	def unary_- : Rational =
		new Rational(-numer, denom)
		
  def - (that: Rational): Rational = this + -that
  
  def < (that: Rational) = numer * that.denom < denom * that.numer
  def max(that: Rational) = if (this < that) that else this
}

object Classes {
	val x = new Rational(1,3)                 //> x  : week02.Rational = 1/3
	x.numer                                   //> res0: Int = 1
	x.denom                                   //> res1: Int = 3
	
	def addRational(r: Rational, s: Rational): Rational =
		new Rational(
			r.numer * s.denom + r.denom * s.numer,
			r.denom * s.denom)        //> addRational: (r: week02.Rational, s: week02.Rational)week02.Rational
		
	def makeStr(r: Rational) = r.numer + "/" + r.denom
                                                  //> makeStr: (r: week02.Rational)String
	
	makeStr(addRational(new Rational(1,2), new Rational(2,3)))
                                                  //> res2: String = 7/6
	
	// Methods
	x + new Rational(1,2)                     //> res3: week02.Rational = 5/6
	
	val y = new Rational(5,7)                 //> y  : week02.Rational = 5/7
	val z = new Rational(3,2)                 //> z  : week02.Rational = 3/2
	
	x - y - z                                 //> res4: week02.Rational = -79/42
	y + y                                     //> res5: week02.Rational = 10/7
	x < y                                     //> res6: Boolean = true
	x max y                                   //> res7: week02.Rational = 5/7
	
	//val strange = (new Rational(1, 0))
	//strange.add(strange)
	new Rational(2)                           //> res8: week02.Rational = 2/1
}