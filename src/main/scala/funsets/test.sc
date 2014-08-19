package funsets

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)
                                                  //> contains: (s: Int => Boolean, elem: Int)Boolean

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = {
    def innerFunction (givenElement: Int) =
      if (elem == givenElement) true
      else false
      innerFunction
  }                                               //> singletonSet: (elem: Int)Int => Boolean
  
 
  val bound = 1000                                //> bound  : Int = 1000

 /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }                                               //> toString: (s: Int => Boolean)String

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }                                               //> printSet: (s: Int => Boolean)Unit

  singletonSet(2)                                 //> res0: Int => Boolean = <function1>
   val s1 = singletonSet(1)                       //> s1  : Int => Boolean = <function1>
    val s2 = singletonSet(2)                      //> s2  : Int => Boolean = <function1>
    val s3 = singletonSet(3)                      //> s3  : Int => Boolean = <function1>
     val s_1 = singletonSet(-1)                   //> s_1  : Int => Boolean = <function1>
       val s_2 = singletonSet(-2)                 //> s_2  : Int => Boolean = <function1>
    contains(s1, 1)                               //> res1: Boolean = true
    contains(s1, 2)                               //> res2: Boolean = false
    contains(s3, 3)                               //> res3: Boolean = true
    contains(s2, 2)                               //> res4: Boolean = true


 /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = {
  	def unionIn(elem : Int) =
  		if (contains(s,elem) || contains(t,elem)) true
  		else false
  	unionIn
  }                                               //> union: (s: Int => Boolean, t: Int => Boolean)Int => Boolean
  
    /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = {
  	def interIn(elem:Int) =
  		if (contains(s,elem) && contains(t,elem)) true
  		else false
  	interIn
  }                                               //> intersect: (s: Int => Boolean, t: Int => Boolean)Int => Boolean
  
   /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
   def diff(s: Set, t: Set): Set = {
   	def diffIn(elem:Int) =
   		if (contains(s,elem) && !contains(t,elem)) true
   		else false
   		
   		diffIn
   }                                              //> diff: (s: Int => Boolean, t: Int => Boolean)Int => Boolean
   
   def neg(x: Int) = x < 0                        //> neg: (x: Int)Boolean
   def double(x: Int) = x*2                       //> double: (x: Int)Int
   
  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = {
     def filterIn(e:Int) =
     		if (contains(s,e) && p(e)) true
     		else false
     filterIn
	}                                         //> filter: (s: Int => Boolean, p: Int => Boolean)Int => Boolean
	
	 /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains(s,a) && !p(a)) false
      else iter(a+1)
    }
    iter(-bound)
  }                                               //> forall: (s: Int => Boolean, p: Int => Boolean)Boolean
  
   /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = {
     def iter(a: Int): Boolean = {
      if (a > bound) false
      else if (contains(s,a) && p(a)) true
      else iter(a+1)
    }
    iter(-bound)
  }                                               //> exists: (s: Int => Boolean, p: Int => Boolean)Boolean
  
   /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = {
  	def mapIn(elem: Int) = {
			def foo(a:Int):Boolean = {
				if (a > bound) false
				else if (contains(s,a) && f(a) == elem) true
				else foo(a+1)
			}
			foo(-bound)
  	}
  	mapIn
  }                                               //> map: (s: Int => Boolean, f: Int => Int)Int => Boolean

  val s = union(s1, s2)                           //> s  : Int => Boolean = <function1>
  contains(s, 1)                                  //> res5: Boolean = true
  contains(s, 3)                                  //> res6: Boolean = false
  contains(s, 2)                                  //> res7: Boolean = true
   printSet(s)                                    //> {1,2}

  val ss = union(s1, s3)                          //> ss  : Int => Boolean = <function1>
   printSet(ss)                                   //> {1,3}
  
  
  val sss = union(s,ss)                           //> sss  : Int => Boolean = <function1>
  val is = intersect(s,sss)                       //> is  : Int => Boolean = <function1>
  printSet(is)                                    //> {1,2}
  
  val d = diff(sss,ss)                            //> d  : Int => Boolean = <function1>
  printSet(d)                                     //> {2}
  

  val nsss = union(sss,s_1)                       //> nsss  : Int => Boolean = <function1>
  val f = filter(nsss, neg)                       //> f  : Int => Boolean = <function1>
  val nnn = union(s_2,s_1)                        //> nnn  : Int => Boolean = <function1>
  printSet(nsss)                                  //> {-1,1,2,3}
  printSet(f)                                     //> {-1}
  printSet(nnn)                                   //> {-2,-1}
  
  
  forall(sss,neg)                                 //> res8: Boolean = false
  forall(nsss,neg)                                //> res9: Boolean = false
  forall(nnn,neg)                                 //> res10: Boolean = true
  exists(nnn,neg)                                 //> res11: Boolean = true
  exists(nsss,neg)                                //> res12: Boolean = true
  exists(sss,neg)                                 //> res13: Boolean = false
  val mm = map(sss,double)                        //> mm  : Int => Boolean = <function1>
  printSet(mm)                                    //> {2,4,6}
}