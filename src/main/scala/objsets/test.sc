package objsets

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
   val set1 = new Empty                           //> set1  : objsets.Empty = objsets.Empty@4a8f79ba
    val set2 = set1.incl(new Tweet("a", "a body", 22))
                                                  //> set2  : objsets.TweetSet = objsets.NonEmpty@4932f4f0
    val set3 = set2.incl(new Tweet("b", "b body", 23))
                                                  //> set3  : objsets.TweetSet = objsets.NonEmpty@3881bb1
    val c = new Tweet("c", "c android Android body", 7)
                                                  //> c  : objsets.Tweet = User: c
                                                  //| Text: c android Android body [7]
    val d = new Tweet("d", "d android body", 13)  //> d  : objsets.Tweet = User: d
                                                  //| Text: d android body [13]
                                       
    val gg = new Tweet("gg", "d Galaxy", 9)       //> gg  : objsets.Tweet = User: gg
                                                  //| Text: d Galaxy [9]
    val gg2 = new Tweet("gg2", "d2 android", 10)  //> gg2  : objsets.Tweet = User: gg2
                                                  //| Text: d2 android [10]
    val gg3 = new Tweet("gg3", "d3 android", 11)  //> gg3  : objsets.Tweet = User: gg3
                                                  //| Text: d3 android [11]
                                                
    
    val set4c = set3.incl(c)                      //> set4c  : objsets.TweetSet = objsets.NonEmpty@5cd20346
    val set4d = set3.incl(d)                      //> set4d  : objsets.TweetSet = objsets.NonEmpty@5fd51845
    val set5 = set4c.incl(d)                      //> set5  : objsets.TweetSet = objsets.NonEmpty@1f2c8b2a
     val set6 = set5.incl(gg2)                    //> set6  : objsets.TweetSet = objsets.NonEmpty@25648263
      val set7 = set6.incl(gg3)                   //> set7  : objsets.TweetSet = objsets.NonEmpty@69ac44c2
    
    val set_gg= set7.incl(gg)                     //> set_gg  : objsets.TweetSet = objsets.NonEmpty@2cd652c1
    
    def asSet(tweets: TweetSet): Set[Tweet] = {
  	  var res = Set[Tweet]()
  	  tweets.foreach(res += _)
  	  res
    }                                             //> asSet: (tweets: objsets.TweetSet)Set[objsets.Tweet]
    
    def asSetList(tweets: TweetList): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }                                               //> asSetList: (tweets: objsets.TweetList)Set[objsets.Tweet]
    
    asSet(set3)                                   //> res0: Set[objsets.Tweet] = Set(User: a
                                                  //| Text: a body [22], User: b
                                                  //| Text: b body [23])
   
    def size(set: TweetSet): Int = asSet(set).size//> size: (set: objsets.TweetSet)Int
  
  
    size(set1)                                    //> res1: Int = 0
	  size(set2)                              //> res2: Int = 1
	  size(set3)                              //> res3: Int = 2
    set1.filter(tw => tw.user == "a")             //> res4: objsets.TweetSet = objsets.Empty@206fcd32
    asSet(set2.filter(tw => tw.user == "a") )     //> res5: Set[objsets.Tweet] = Set(User: a
                                                  //| Text: a body [22])
    asSet(set3.filter(tw => tw.user == "a") )     //> res6: Set[objsets.Tweet] = Set(User: a
                                                  //| Text: a body [22])
    size(set4c.union(set4d))                      //> res7: Int = 4
    val trends = set5.descendingByRetweet         //> trends  : objsets.TweetList = objsets.Cons@1578fd24
                 
    
    
		trends.head.user                  //> res8: String = b
 	  trends.tail.head.user                   //> res9: String = a
 	  trends.tail.tail.head.user              //> res10: String = d
 	  
 	  
 	  val tt = GoogleVsApple.search(set_gg,GoogleVsApple.google)
                                                  //> search start
                                                  //| found=11
                                                  //| found=10
                                                  //| found=13
                                                  //| found=7
                                                  //| found=7
                                                  //| found=9
                                                  //| tt  : objsets.TweetSet = objsets.NonEmpty@9e5941c
 	  asSet(tt)                               //> res11: Set[objsets.Tweet] = Set(User: gg3
                                                  //| Text: d3 android [11], User: c
                                                  //| Text: c android Android body [7], User: gg2
                                                  //| Text: d2 android [10], User: d
                                                  //| Text: d android body [13], User: gg
                                                  //| Text: d Galaxy [9])
 	  val desc_tt = tt.descendingByRetweet    //> desc_tt  : objsets.TweetList = objsets.Cons@2ee784c0
    asSetList(desc_tt)                            //> res12: Set[objsets.Tweet] = Set(User: gg3
                                                  //| Text: d3 android [11], User: c
                                                  //| Text: c android Android body [7], User: gg2
                                                  //| Text: d2 android [10], User: d
                                                  //| Text: d android body [13], User: gg
                                                  //| Text: d Galaxy [9])
                           
                                                  //
    desc_tt.head.retweets                         //> res13: Int = 13
    desc_tt.tail.head.retweets                    //> res14: Int = 11
    desc_tt.tail.tail.head.retweets               //> res15: Int = 10
 		desc_tt.tail.tail.tail.head.retweets
                                                  //> res16: Int = 9
 		desc_tt.tail.tail.tail.tail.head.retweets
                                                  //> res17: Int = 7
}