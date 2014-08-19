package recfun
import common._

object Main {
  
   def bal(chars: List[Char], cnt: Int) : Boolean = {
  	
  	if (cnt <0) return false
  	if (chars.isEmpty) {
  	   if (cnt == 0)
  			return true
  	  else
  	  	return false
  	}
  	
  	if (chars.head == ')' && chars.tail.isEmpty) {
  		if (cnt == 1)
  			return true
  		else
  			return false
  	}
  	if (chars.head == '(' && chars.tail.isEmpty)
  		return false
  		
  	if (chars.head == '(') 
  		//return false
  		return bal(chars.tail,cnt+1)
  	else if (chars.head == ')') {
  		if (cnt == 0) return false
  		else
  			return bal(chars.tail,cnt-1)
  	}
  		
  	bal(chars.tail,cnt)
  }                                               //> bal2: (chars: List[Char], cnt: Int)Boolean
   
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) true
  	else bal(chars,0)
  }                                               //> balance: (chars: List[Char])Boolean
  
  
  
  
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    
    println("example1 : " + balance("(just an) example".toList)  )           //> res0: Boolean = false
    /*
    balance("(just an)) example".toList)           //> res1: Boolean = false
    balance("(just an)(( example".toList)         //> res2: Boolean = false
    balance("(".toList)                          //> res3: Boolean = false
    balance(")".toList)                          //> res4: Boolean = false
  
   balance("".toList)  */
  }
  
  def powerTwo(r:Int) : Int = {
    2 ^ r
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else if (c == r ) 1
    else if (c+1 > r) 0
    else {
      pascal(c-1,r-1) + pascal(c,r-1)
    }
  }    
  

  

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) return 1
    else if (money > 0 && coins.isEmpty) return 0
    else if (money < 0 ) return 0
    else return countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }   

}
