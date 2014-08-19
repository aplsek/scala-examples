package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class implements a ScalaTest test suite for the methods in object
 * `Lists` that need to be implemented as part of this assignment. A test
 * suite is simply a collection of individual tests for some specific
 * component of a program.
 *
 * A test suite is created by defining a class which extends the type
 * `org.scalatest.FunSuite`. When running ScalaTest, it will automatically
 * find this class and execute all of its tests.
 *
 * Adding the `@RunWith` annotation enables the test suite to be executed
 * inside eclipse using the built-in JUnit test runner.
 *
 * You have two options for running this test suite:
 * 
 * - Start the sbt console and run the "test" command
 * - Right-click this file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class RecfunSuite extends FunSuite {

  

  /**
   * Now we finally write some tests for the list functions that have to be
   * implemented for this assignment. We fist import all members of the
   * `List` object.
   */ 
  import Main._
  

  /**
   * We only provide two very basic tests for you. Write more tests to make
   * sure your `sum` and `max` methods work as expected.
   *
   * In particular, write tests for corner cases: negative numbers, zeros,
   * empty lists, lists with repeated elements, etc.
   *
   * It is allowed to have multiple `assert` statements inside one test,
   * however it is recommended to write an individual `test` statement for
   * every tested aspect of a method.
   */
  test("sum2 of a few numbers") {
    assert(countChange(4,List(1,2)) === 3)
  }
  
   test("sum 1of a few numbers") {
    assert(countChange(4,List(2)) === 1)
  }
  
    test("sum 33of a few numbers") {
    assert(countChange(5,List(2,3)) === 1)
  }
   
  test("max of a few number2222s") {
    assert( countChange(4,List(1,2,3)) === 4)
  }
  
   test("sum of a few numbers2") {
    assert(countChange(4,List(1,2,3,4)) === 5)
  }
   
   test("ales1") {
    assert( countChange(6,List(1,2,3)) === 7)
  }
   
   
  test("b-negavtiv111111e") {
     assert(balance("(".toList)  == false )
  }
  
   test("b-negavtive2") {
     assert(balance("".toList)  == true )
  }
   
   test("balance-null") {
     assert(balance(null)  == true )
  }
 
  test("b-negavtive4") {
     assert(balance(")".toList)    == false )
  }
    
  test("b2-negavtive") {
     assert(balance(")".toList)  == false )  
  }
    
  test("b-smile") {
     assert(balance(":-)".toList)  == false )  
  }
    
  test("b4-negavtive") {
     assert(balance("())(".toList)    == false )
  }
   
  test("b4-example-ok") {
     assert(balance("(just an) example".toList) == true )
  }
  
  test("b44-negavtive") {
     assert(balance("(if (zero? x) max (/1 x))".toList)   == true ) 
  }
  
  test("b44-neg33avtive") {
     assert(balance("I told him (that it's not (yet) done). (But he wasn't listening)".toList)    == true )
  }
  
  
    println("Pascal's Triangle")
    balance("(just an) example".toList)             //> res0: Boolean = false
    balance("(just an)) example".toList)           //> res1: Boolean = false
    balance("(just an)(( example".toList)         //> res2: Boolean = false
    balance("(".toList)                          //> res3: Boolean = false
    balance(")".toList)                          //> res4: Boolean = false
  
   balance("".toList)  

}

