package forcomp

import forcomp.Anagrams._

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val w = "tTee..,ssstt"                          //> w  : String = tTee..,ssstt
  val l = w.toList                                //> l  : List[Char] = List(t, T, e, e, ., ., ,, s, s, s, t, t)
  val xx = w.toLowerCase.filter(_.isLetter).groupBy(identity)
                                                  //> xx  : scala.collection.immutable.Map[Char,String] = Map(e -> ee, t -> tttt, 
                                                  //| s -> sss)
  val x = w.toLowerCase.filter(_.isLetter).groupBy(identity)
                                                  //> x  : scala.collection.immutable.Map[Char,String] = Map(e -> ee, t -> tttt, s
                                                  //|  -> sss)
  val xxx = w.toLowerCase.filter(_.isLetter).toList.groupBy((element:Char) => element).mapValues(_.length).toList.sorted
                                                  //> xxx  : List[(Char, Int)] = List((e,2), (s,3), (t,4))
  
  val mm = l.groupBy((element:Char) => element)   //> mm  : scala.collection.immutable.Map[Char,List[Char]] = Map(e -> List(e, e),
                                                  //|  s -> List(s, s, s), . -> List(., .), T -> List(T), t -> List(t, t, t), , ->
                                                  //|  List(,))
  l.mkString                                      //> res0: String = tTee..,ssstt
  
  mm.toList                                       //> res1: List[(Char, List[Char])] = List((e,List(e, e)), (s,List(s, s, s)), (.,
                                                  //| List(., .)), (T,List(T)), (t,List(t, t, t)), (,,List(,)))
  // w.toLowerCase.filter(_.isLetter).groupBy(identity).mapValues(_.length).toList.sorted
  
   val w2 = "I"                                   //> w2  : String = I
   val w3 = "love"                                //> w3  : String = love
   val w4 = "you"                                 //> w4  : String = you
    val w5 = "ouy"                                //> w5  : String = ouy
   
   val ll = List[String](w2,w3,w4,w5)             //> ll  : List[String] = List(I, love, you, ouy)
   //ll.mkString
   //ll.groupBy((element:Word) => wordOccurrences(element))
  
  
  
   val ww = "abc"                                 //> ww  : String = abc
   val li = ww.toList                             //> li  : List[Char] = List(a, b, c)
   val y = "x"                                    //> y  : String = x
   val lx = "x".toList                            //> lx  : List[Char] = List(x)
   
   val lll = List[List[Char]](w4.toList,w3.toList)//> lll  : List[List[Char]] = List(List(y, o, u), List(l, o, v, e))
   
   //li map (lx + _)
   lll map (e => y  :: e)                         //> res2: List[List[Any]] = List(List(x, y, o, u), List(x, l, o, v, e))
   lll map (y  :: _)                              //> res3: List[List[Any]] = List(List(x, y, o, u), List(x, l, o, v, e))
   
   def powerAcc(l: List[Char], acc : List[List[Char]]):  List[List[Char]] = {
     l match {
     		case Nil => acc
     		case a :: as => powerAcc(as, acc ::: (acc map (a :: _)))
     }
   }                                              //> powerAcc: (l: List[Char], acc: List[List[Char]])List[List[Char]]

   //powerAcc(li,Nil :: Nil)
   def wordOccurrences(w: Word): Occurrences = {
	  w.toLowerCase.filter(_.isLetter).toList.groupBy((element:Char) => element).mapValues(_.length).toList.sorted
   }                                              //> wordOccurrences: (w: forcomp.Anagrams.Word)forcomp.Anagrams.Occurrences
   
   
   def remove(x: Occurrences, y: (Char,Int)) : Occurrences = {
   	 x match {
   	 		case Nil => Nil
   	 		case (y._1,y._2) :: xs => xs
   	 		case (y._1,n) :: xs => (y._1, n - y._2) :: xs
   	 		case xh :: xs => xh :: remove(xs,y)
   	 }
   }                                              //> remove: (x: forcomp.Anagrams.Occurrences, y: (Char, Int))forcomp.Anagrams.O
                                                  //| ccurrences
    
   def subtract(x: Occurrences, y: Occurrences): Occurrences = {
      y match {
      case Nil =>  x
      case (e,n) :: ys => subtract(remove(x,(e,n)) , ys)
      }
   }                                              //> subtract: (x: forcomp.Anagrams.Occurrences, y: forcomp.Anagrams.Occurrences
                                                  //| )forcomp.Anagrams.Occurrences
   
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    occurrences match {
    case Nil => Nil
    case (e,1) :: ocs => List((e,1)) :: combinations(ocs)
    case (e,2) :: ocs => List((e,2)) :: combinations(ocs)
    case (e,n) :: ocs => List((e,1)) :: combinations(subtract(occurrences,List[(Char,Int)]((e,n))))
    }
  }                                               //> combinations: (occurrences: forcomp.Anagrams.Occurrences)List[forcomp.Anagr
                                                  //| ams.Occurrences]
   
   // remove(occ,('t',2))
   val occ = wordOccurrences(w)                   //> occ  : forcomp.Anagrams.Occurrences = List((e,2), (s,3), (t,4))
   subtract(occ,List(('t',2),('s',2)))            //> res4: forcomp.Anagrams.Occurrences = List((e,2), (s,1), (t,2))
   
   combinations(occ)                              //> res5: List[forcomp.Anagrams.Occurrences] = List(List((e,2)), List((s,1)), L
                                                  //| ist((t,1)))
     
  // dictionary.groupBy((element:Word) => wordOccurrences(element))
     
   //Anagrams.dictionaryByOccurrences
     
  //mm.toList.groupBy(element:9Char,List[Char]) => element)
 // mm.toList map ((e,l):(Char,List(Char)) => (e,l.lenght)) {}
}