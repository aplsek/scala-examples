package recfun

object coins {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  /*
   def countChange(m: Int, c: List[Int]): Int = {
   	if (m == 0) return 1
   	if (c.isEmpty) return 0
   	
   	return countChange(m,c.tail) + countChange(m - c.head, c.tail)
   }   */
   
    /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) return 1
    else if (money > 0 && coins.isEmpty) return 0
    else if (money < 0 ) return 0
    else return countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }                                               //> countChange: (money: Int, coins: List[Int])Int
   
   countChange(4,List(1,2))                       //> res0: Int = 3
   countChange(4,List(1,2,3))                     //> res1: Int = 4
   countChange(4,List(1,2,3,4))                   //> res2: Int = 5
   countChange(6,List(1,2,3))                     //> res3: Int = 7
   countChange(6,List(1,2,5))                     //> res4: Int = 5
}