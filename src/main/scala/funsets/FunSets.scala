package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = {
    def innerFunction (givenElement: Int) =
      if (elem == givenElement) true
      else false
      innerFunction
  }  

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
  
   val bound = 1000  
  
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
   }   

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
  } 

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
