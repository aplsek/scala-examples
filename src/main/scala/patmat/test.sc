package patmat

import patmat.Huffman._

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
 
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
                                                  //> t1  : patmat.Huffman.Fork = Fork(Leaf(a,2),Leaf(b,3),List(a, b),5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
                                                  //> t2  : patmat.Huffman.Fork = Fork(Fork(Leaf(a,2),Leaf(b,3),List(a, b),5),Leaf
                                                  //| (d,4),List(a, b, d),9)
   weight(t1)                                     //> res0: Int = 5
   chars(t2)                                      //> res1: List[Char] = List(a, b, d)
  
  
  makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3)))
                                                  //> res2: List[patmat.Huffman.Leaf] = List(Leaf(e,1), Leaf(t,2), Leaf(x,3))
  
  val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
                                                  //> leaflist  : List[patmat.Huffman.Leaf] = List(Leaf(e,1), Leaf(t,2), Leaf(x,4)
                                                  //| )
  
  times(List('a','b','c','a','b','b'))            //> res3: List[(Char, Int)] = List((c,1), (a,2), (b,3))
  
  convert(t2)                                     //> res4: patmat.Huffman.CodeTable = List((a,List(0, 0)), (b,List(0, 1)), (d,Lis
                                                  //| t(1)))
  
  
  val sec = List('a','b','a','d','a','b','b')     //> sec  : List[Char] = List(a, b, a, d, a, b, b)
  quickEncode(t2)(sec)                            //> res5: List[patmat.Huffman.Bit] = List(0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1)
                                                  //| 
  encode(t2)(sec)                                 //> res6: List[patmat.Huffman.Bit] = List(0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1)
                                                  //| 
  
  
  decodedSecret                                   //> res7: List[Char] = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)

  
  
  
  
  
  decode(t1, encode(t1)("ab".toList))             //> res8: List[Char] = List(a, b)
  
  decode(t2, encode(t2)("abdd".toList))           //> res9: List[Char] = List(a, b, d, d)
 
  encode(frenchCode)("huffmanestcool".toList)     //> res10: List[patmat.Huffman.Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0
                                                  //| , 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1,
                                                  //|  0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)
  decode(frenchCode, encode(frenchCode)("huffmanestcool".toList))
                                                  //> res11: List[Char] = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)
  
  val l = "huffmanestcool".toList                 //> l  : List[Char] = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)
  val ll = times(l)                               //> ll  : List[(Char, Int)] = List((l,1), (c,1), (t,1), (s,1), (e,1), (n,1), (a,
                                                  //| 1), (m,1), (u,1), (h,1), (f,2), (o,2))
  val ttt = makeOrderedLeafList(ll)               //> ttt  : List[patmat.Huffman.Leaf] = List(Leaf(l,1), Leaf(c,1), Leaf(t,1), Lea
                                                  //| f(s,1), Leaf(e,1), Leaf(n,1), Leaf(a,1), Leaf(m,1), Leaf(u,1), Leaf(h,1), Le
                                                  //| af(f,2), Leaf(o,2))
  combine(ttt)                                    //> res12: List[patmat.Huffman.CodeTree] = List(Fork(Fork(Fork(Leaf(u,1),Leaf(h,
                                                  //| 1),List(u, h),2),Fork(Leaf(f,2),Leaf(o,2),List(f, o),4),List(u, h, f, o),6),
                                                  //| Fork(Fork(Fork(Leaf(l,1),Leaf(c,1),List(l, c),2),Fork(Leaf(t,1),Leaf(s,1),Li
                                                  //| st(t, s),2),List(l, c, t, s),4),Fork(Fork(Leaf(e,1),Leaf(n,1),List(e, n),2),
                                                  //| Fork(Leaf(a,1),Leaf(m,1),List(a, m),2),List(e, n, a, m),4),List(l, c, t, s, 
                                                  //| e, n, a, m),8),List(u, h, f, o, l, c, t, s, e, n, a, m),14))
  val codeTree = createCodeTree("huffmanestcool".toList)
                                                  //> codeTree  : patmat.Huffman.CodeTree = Fork(Fork(Fork(Leaf(u,1),Leaf(h,1),Lis
                                                  //| t(u, h),2),Fork(Leaf(f,2),Leaf(o,2),List(f, o),4),List(u, h, f, o),6),Fork(F
                                                  //| ork(Fork(Leaf(l,1),Leaf(c,1),List(l, c),2),Fork(Leaf(t,1),Leaf(s,1),List(t, 
                                                  //| s),2),List(l, c, t, s),4),Fork(Fork(Leaf(e,1),Leaf(n,1),List(e, n),2),Fork(L
                                                  //| eaf(a,1),Leaf(m,1),List(a, m),2),List(e, n, a, m),4),List(l, c, t, s, e, n, 
                                                  //| a, m),8),List(u, h, f, o, l, c, t, s, e, n, a, m),14)
  decode(codeTree, encode(codeTree)("huffmanestcool".toList))
                                                  //> res13: List[Char] = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)
  
                                       
}