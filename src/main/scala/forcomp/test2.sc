package forcomp

import forcomp.Anagrams._

object test2 {

	 val w = "tTee..,ssstt"                   //> w  : String = tTee..,ssstt

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
   }                                              //> remove: (x: forcomp.Anagrams.Occurrences, y: (Char, Int))forcomp.Anagrams.Oc
                                                  //| currences
    
   def subtract(x: Occurrences, y: Occurrences): Occurrences = {
      y match {
      case Nil =>  x
      case (e,n) :: ys => subtract(remove(x,(e,n)) , ys)
      }
   }                                              //> subtract: (x: forcomp.Anagrams.Occurrences, y: forcomp.Anagrams.Occurrences)
                                                  //| forcomp.Anagrams.Occurrences

  
  def add(x : (Char,Int), ll: List[Occurrences]): List[Occurrences] = {
    ll match {
    case Nil => List[Occurrences](List(x))
    case lh :: lls => {
        val y = x :: lh
  		  val z = List[Occurrences](y)
    		z ::: add(x,lls)
    	}
    }
   
  }                                               //> add: (x: (Char, Int), ll: List[forcomp.Anagrams.Occurrences])List[forcomp.An
                                                  //| agrams.Occurrences]

  
   //type Occurrences = List[(Char, Int)]
  def combinations(occurrences: Occurrences): List[Occurrences] = {
   combinationsAcc(occurrences)  ::: List[Occurrences](Nil)
  }                                               //> combinations: (occurrences: forcomp.Anagrams.Occurrences)List[forcomp.Anagr
                                                  //| ams.Occurrences]
   
   def combinationsAcc(occurrences: Occurrences): List[Occurrences] = {
    occurrences match {
    	case Nil => List[Occurrences]()
    	case (e,n) :: ocs => {
    		 val occAcc = combinationsAcc(ocs)
    	   var res  = occAcc
    	   for (i <- 1 until n+1) {
    	   			res = res :::  add((e,i),occAcc)
    	   }
    	   res
    	}
    }
  }                                               //> combinationsAcc: (occurrences: forcomp.Anagrams.Occurrences)List[forcomp.An
                                                  //| agrams.Occurrences]
   
  
   // remove(occ,('t',2))
   val occ = wordOccurrences(w)                   //> occ  : forcomp.Anagrams.Occurrences = List((e,2), (s,3), (t,4))
   val occ2  = List(('a',2),('b',2))              //> occ2  : List[(Char, Int)] = List((a,2), (b,2))
   // subtract(occ,occ2)
   
   add (('a',1), List[Occurrences](occ,occ2))     //> res0: List[forcomp.Anagrams.Occurrences] = List(List((a,1), (e,2), (s,3), (
                                                  //| t,4)), List((a,1), (a,2), (b,2)), List((a,1)))
   
  val zz= combinations(occ2)                      //> zz  : List[forcomp.Anagrams.Occurrences] = List(List((b,1)), List((b,2)), L
                                                  //| ist((a,1), (b,1)), List((a,1), (b,2)), List((a,1)), List((a,2), (b,1)), Lis
                                                  //| t((a,2), (b,2)), List((a,2)), List())
  zz.size                                         //> res1: Int = 9
   
  zz ::: List[Occurrences](Nil)                   //> res2: List[forcomp.Anagrams.Occurrences] = List(List((b,1)), List((b,2)), L
                                                  //| ist((a,1), (b,1)), List((a,1), (b,2)), List((a,1)), List((a,2), (b,1)), Lis
                                                  //| t((a,2), (b,2)), List((a,2)), List(), List())
   
  val sen = List[Word]("I","love", "you")         //> sen  : List[forcomp.Anagrams.Word] = List(I, love, you)
  val anag1 = sentenceAnagrams(sen)               //> anag1  : List[forcomp.Anagrams.Sentence] = List(List(Lev, Io, you), List(Io
                                                  //| , Lev, you), List(olive, you), List(Lev, you, Io), List(you, Lev, Io), List
                                                  //| (Io, you, Lev), List(you, Io, Lev), List(you, olive))
  anag1.size                                      //> res3: Int = 8
  
  
  val sen2 = List[Word]("yes","man")              //> sen2  : List[forcomp.Anagrams.Word] = List(yes, man)
  val anag2 = sentenceAnagrams(sen2)              //> anag2  : List[forcomp.Anagrams.Sentence] = List(List(as, en, my), List(en, 
                                                  //| as, my), List(sane, my), List(Sean, my), List(man, yes), List(as, my, en), 
                                                  //| List(my, as, en), List(say, men), List(men, say), List(en, my, as), List(my
                                                  //| , en, as), List(yes, man), List(my, sane), List(my, Sean))
  
 
 
  // anag2 == 14
  anag2.size                                      //> res4: Int = 14
  
  
  
  
  
  
  
  
  
  
   
}