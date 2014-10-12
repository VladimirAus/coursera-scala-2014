package week01

object assignment_e2_balancing {
	def balance(chars: List[Char]): Boolean = {
		// chars.isEmpty: Boolean returns whether a list is empty
		// chars.head: Char returns the first element of the list
		// chars.tail: List[Char] returns the list without the first element
		
		def balanceIter(chrs: List[Char], balNum: Int): Boolean = {
			// println(balNum)
			if (balNum < 0) false
			else if (chrs.isEmpty) balNum == 0
			else
				if (chrs.head == "(".toList.head) balanceIter(chrs.tail, balNum + 1)
				else if (chrs.head == ")".toList.head) balanceIter(chrs.tail, balNum - 1)
				else  balanceIter(chrs.tail, balNum)
		}
		
		balanceIter(chars, 0)
	}                                         //> balance: (chars: List[Char])Boolean
	
	// Testing
	balance("(if (zero? x) max (/ 1 x))".toList) == true
                                                  //> res0: Boolean = true
	balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList) == true
                                                  //> res1: Boolean = true
	balance(":-)".toList) == false            //> res2: Boolean = true
	balance("())(".toList) == false           //> res3: Boolean = true
}