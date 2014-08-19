package recfun

object triangle {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  def powerTwo(r:Int) : Int = {
    2 ^ r
  }                                               //> powerTwo: (r: Int)Int

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else if (c == r ) 1
    else if (c+1 > r) 0
    else {
     // Int re = pascal(c-1,r-1) + pascal(c,r-1)
      //println(pascal(c-1,r-1) + pascal(c,r-1))
      //re
      pascal(c-1,r-1) + pascal(c,r-1)
      //22
    }
  }                                               //> pascal: (c: Int, r: Int)Int
  /*
  //1, 2, 3
  pascal(0,2)
  pascal(0,0)
  pascal(0,1)
  pascal(1,0)
  pascal(1,1)
  //2
  pascal(1,2)
  //3
  pascal(1,3)
  pascal(3,3)
  pascal(2,4)
  pascal(4,4)
  pascal(3,4)
  */
  
  def bal(chars: List[Char], cnt: Int) : Boolean = {
  	
  	if (cnt <0) return false
  	if (chars.isEmpty) {
  		if (cnt == 0)
  			return true
  	  else
  	  	return false
  	}
  	/*
  	if (chars.head == ")" && chars.tail.isEmpty) {
  		if (cnt == 1)
  			return true
  		else
  			return false
  	}
  	if (chars.head == "(" && chars.tail.isEmpty)
  		return false
  		*/
  	if (chars.head == '(')
  		//return false
  		return bal(chars.tail,cnt+1)
  	else if (chars.head == ')') {
  		if (cnt == 0) return false
  		else
  			return bal(chars.tail,cnt-1)
  	}
  		
  	return bal(chars.tail,cnt)
  }                                               //> bal: (chars: List[Char], cnt: Int)Boolean
   
  def balance(chars: List[Char]): Boolean = {
  	if (chars.isEmpty) true
  	else bal(chars,0)
  }                                               //> balance: (chars: List[Char])Boolean
  
  balance("(just an) example".toList)             //> res0: Boolean = true
   balance("())(".toList)                         //> res1: Boolean = false
    balance(":-)".toList)                         //> res2: Boolean = false
 balance("I told him (that its not (yet) done). (But he wasnt listening)".toList)
                                                  //> res3: Boolean = true
     balance("(if (zero? x) max (/ 1 x))".toList) //> res4: Boolean = true
   balance("()".toList)                           //> res5: Boolean = true
    balance("asdfasd (sdf(9909(9)09))  asdf ((  dgsdfg )) ()".toList)
                                                  //> res6: Boolean = true
      balance("asdfasd (sdf(9909(9)09))  asdf ((  dgsdfg )) ())".toList)
                                                  //> res7: Boolean = false
   balance("".toList)                             //> res8: Boolean = true
}