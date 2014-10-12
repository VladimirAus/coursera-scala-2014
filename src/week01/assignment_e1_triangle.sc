package week01

object assignment_e1_triangle {
	def pascal(c: Int, r: Int): Int = {
		if ((c > r) || (c < 0) || (r < 0)) 0
		else if ((r == 0) || (c == 0) || (r == c)) 1
		else
			pascal(c, r-1) + pascal (c-1, r-1)
	}                                         //> pascal: (c: Int, r: Int)Int
	
	pascal(0,2)==1                            //> res0: Boolean = true
	pascal(1,2)==2                            //> res1: Boolean = true
	pascal(1,3)==3                            //> res2: Boolean = true
	pascal(1,4)==4                            //> res3: Boolean = true
	pascal(2,4)==6                            //> res4: Boolean = true
	pascal(3,4)==4                            //> res5: Boolean = true
}