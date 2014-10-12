package week01

object assignment_e3_change {
	def countChange(money: Int, coins: List[Int]): Int = {
		def countChangeIter(coinsLeft: List[Int], moneyIter: Int, moneyLeft: Int): Int = {
			// println(coinsLeft, moneyIter, moneyLeft)
			if (coinsLeft.isEmpty) 0
			else
				if (moneyIter * coinsLeft.head - moneyLeft == 0) 1
				else if (moneyIter * coinsLeft.head > moneyLeft) 0
				else
					countChangeIter(coinsLeft, moneyIter+1, moneyLeft) + countChangeIter(coinsLeft.tail, 0, moneyLeft - (moneyIter*coinsLeft.head))
		
		}
		countChangeIter(coins, 0, money)
		
	}                                         //> countChange: (money: Int, coins: List[Int])Int
	
	// Tesing
	countChange(4, List(1,2)) == 3            //> res0: Boolean = true
	countChange(4, List(1,2,3)) == 4          //> res1: Boolean = true
	
	10 / 3                                    //> res2: Int(3) = 3
}