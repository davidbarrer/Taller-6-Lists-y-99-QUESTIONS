package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListsSpec extends AnyFlatSpec with Matchers {

  //Taller 6

  //Exercise 1.
  "Applying the subs() function with the parameters List(1,2)" should "return List(List(),List(2),List(1),List(1,2))" in {
    Lists.subs(List(1,2)) shouldEqual List(List(),List(2),List(1),List(1,2))
  }

  "Applying the subs() function with the parameters List(\"a\",\"b\",\"c\")" should "return List(List(),List(\"a\"),List(\"b\"),List(\"c\"),\nList(\"a\",\"b\"),List(\"a\",\"c\"),List(\"b\",\"c\"),\nList(\"a\",\"b\",\"c\"))" in {
    Lists.subs(List("a","b","c")) shouldEqual List(List(),List("c"),List("b"),List("b","c"),List("a"),List("a","c"),List("a","b"),List("a","b","c"))
  }


  //Exercise 2.

  "Applying the perms function with the parameters List(\"a\",\"b\",\"c\")" should "return List(List(\"a\", \"b\", \"c\"), List(\"b\", \"a\", \"c\"), List(\"b\", \"c\", \"a\"), List(\"a\", \"c\", \"b\"), List(\"c\", \"a\", \"b\"), List(\"c\", \"b\", \"a\"))" in {
    Lists.perms(List("a","b","c")) shouldEqual List(List("a", "b", "c"), List("b", "a", "c"), List("b", "c", "a"), List("a", "c", "b"), List("c", "a", "b"), List("c", "b", "a"))
  }


  "Applying the perms function with the parameters List(2,3,4)" should "return List(List(2,3,4),List(3,2,4),\nList(3,4,2),List(2,4,3),\nList(4,2,3),List(4,3,2))" in {
    Lists.perms(List(2,3,4)) shouldEqual List(List(2,3,4),List(3,2,4),List(3,4,2),List(2,4,3),List(4,2,3),List(4,3,2))
  }




  //99 Questions. from 1 to 28

  //PROBLEM 1
  "Applying the lastElementList() function with the parameters List(\"x\",\"y\",\"z\")" should "return z" in {
    Lists.lastElementList(List("x","y","z")) shouldEqual "z"
  }

  "Applying the lastElementList() function with the parameters List(2,3,6,8,1)" should "return 1" in {
    Lists.lastElementList(List(2,3,6,8,1)) shouldEqual 1
  }

  //PROBLEM 2
  "Applying the lastButOneElement() function with the parameters List(1, 2, 3, 4)" should "return 3" in {
    Lists.lastButOneElement(List(1, 2, 3, 4)) shouldEqual 3
  }

  "Applying the lastButOneElement() function with the parameters List(\"x\",\"a\",\"b\",\"x\",\"a\",\"b\",\"y\",\"z\")" should "" +
    "return y" in {
    Lists.lastButOneElement(List("x","a","b","x","a","b","y","z")) shouldEqual "y"
  }

  //PROBLEM BONUS
  "Applying the lastAndLastButOne() function with the parameters List(1, 2, 3, 4, 1, 2, 3, 4)" should "return (3,4)" in {
    Lists.lastAndLastButOne(List(1,2,3,4,1,2,3,4)) shouldEqual List(3,4)
  }

  "Applying the lastAndLastButOne() function with the parameters List(\"x\",\"a\",\"b\",\"x\",\"a\",\"b\",\"y\",\"z\")" should "" +
    "return (y,z)" in {
    Lists.lastAndLastButOne(List("x","a","b","x","a","b","y","z")) shouldEqual List("y","z")
  }


  //PROBLEM 3
  "Applying the kElement() function with the parameters List(\"a\",\"b\",\"c\",\"d\",\"e\") and with pos = 3" should "" +
    "return c" in {
    Lists.kElement(List("a","b","c","d","e"),3) shouldEqual "c"
  }

  "Applying the kElement() function with the parameters List(\"a\",\"b\",\"c\",\"d\",\"e\") and with pos = 1" should "" +
    "return a" in {
    Lists.kElement(List("a","b","c","d","e"),1) shouldEqual "a"
  }

  "Applying the kElement() function with the parameters List(\"a\",\"b\",\"c\",\"d\",\"e\") and with pos = 5" should "" +
    "return e" in {
    Lists.kElement(List("a","b","c","d","e"),5) shouldEqual "e"
  }



  //PROBLEM 4
  "Applying the myLengthFL() function with the parameters List(1, 2, 3, 1, 2, 3, 1, 2, 3) " should "" +
    "return 9" in {
    Lists.myLengthFL(List(1, 2, 3, 1, 2, 3, 1, 2, 3) ) shouldEqual 9
  }

  "Applying the myLengthFL() function with the parameters List() " should "" +
    "return 0" in {
    Lists.myLengthFL(List() ) shouldEqual 0
  }

  "Applying the myLengthFL() function with the parameters List(\"a\",\"b\",\"c\",\"d\",\"e\") " should "" +
    "return 5" in {
    Lists.myLengthFL(List("a","b","c","d","e") ) shouldEqual 5
  }



  //PROBLEM 5
  "Applying the myReverse() function with the parameters List(1, 2, 3, 1, 2, 3, 1, 2, 3) " should "" +
    "return List(3, 2, 1, 3, 2, 1, 3, 2, 1)" in {
    Lists.myReverse(List(1, 2, 3, 1, 2, 3, 1, 2, 3) ) shouldEqual List(3, 2, 1, 3, 2, 1, 3, 2, 1)
  }

  "Applying the myReverse() function with the parameters List(\"a\",\"b\",\"c\",\"d\",\"e\") " should "" +
    "return List(\"e\",\"d\",\"c\",\"b\",\"a\")" in {
    Lists.myReverse(List("a","b","c","d","e") ) shouldEqual List("e","d","c","b","a")
  }


  //PROBLEM 6
  "Applying the pal() function with the parameters List(1, 2, 2, 4, 8, 3, 8, 4, 2, 2, 1) " should "" +
    "return true" in {
    Lists.pal(List(1, 2, 2, 4, 8, 3, 8, 4, 2, 2, 1)) shouldEqual true
  }

  "Applying the pal() function with the parameters List(1, 2, 2, 4, 8, 3, 8, 4, 2, 2, 1, 1) " should "" +
    "return false" in {
    Lists.pal(List(1, 2, 2, 4, 8, 3, 8, 4, 2, 2, 1, 1)) shouldEqual false
  }

  "Applying the pal() function with the parameters List(\"a\", \"b\", \"b\", \"c\", \"d\", \"e\", \"d\", \"c\", \"b\", \"b\", \"a\")" +
    "" should "return true" in {
    Lists.pal(List("a", "b", "b", "c", "d", "e", "d", "c", "b", "b", "a")) shouldEqual true
  }

  //PROBLEM 7


  //PROBLEM 8
  "Applying the compress() function with the parameters List(1, 1, 1, 2, 2, 2, 3, 3, 4, 4)" +
    "" should "return List(1, 2, 3, 4)" in {
    Lists.compress(List(1, 1, 1, 2, 2, 2, 3, 3, 4, 4)) shouldEqual List(1, 2, 3, 4)
  }

  "Applying the compress() function with the parameters List(1, 2, 3, 4, 5, 6, 7, 8)" +
    "" should "return List(1, 2, 3, 4, 5, 6, 7, 8)" in {
    Lists.compress(List(1, 2, 3, 4, 5, 6, 7, 8)) shouldEqual List(1, 2, 3, 4, 5, 6, 7, 8)
  }


  "Applying the compress() function with the parameters List(\"a\", \"b\",\"b\" , \"c\", \"d\", \"d\", \"d\", \"d\", \"e\", \"f\")" +
    "" should "return List(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\")" in {

    Lists.compress(List("a", "b","b" , "c", "d", "d", "d", "d", "e", "f")) shouldEqual  List("a", "b", "c", "d", "e", "f")
  }


  //PROBLEM 9
  "Applying the pack() function with the parameters List(\"a\", \"b\",\"b\" , \"c\", \"d\", \"d\", \"d\", \"d\", \"e\", \"f\")" +
    "" should "return List(List(\"a\"), List(\"b\",\"b\"), List(\"c\"), List(\"d\",\"d\",\"d\",\"d\"), List(\"e\"), List(\"f\"))" in {

    Lists.pack(List("a", "b","b" , "c", "d", "d", "d", "d", "e", "f")) shouldEqual  List(List("a"), List("b","b"), List("c"), List("d","d","d","d"), List("e"), List("f"))
  }

  "Applying the pack() function with the parameters List(\"a\",\"b\" , \"c\",\"d\", \"e\", \"f\")" +
    "" should "return List(List(\"a\"), List(\"b\"), List(\"c\"), List(\"d\"), List(\"e\"), List(\"f\"))" in {

    Lists.pack(List("a","b" , "c","d", "e", "f")) shouldEqual  List(List("a"), List("b"), List("c"), List("d"), List("e"), List("f"))
  }

  "Applying the pack() function with the parameters List(1, 1, 1, 2, 2, 2, 3, 3, 4, 4)" +
    "" should "return List(List(1,1,1), List(2,2,2), List(3,3), List(4,4))" in {

    Lists.pack(List(1, 1, 1, 2, 2, 2, 3, 3, 4, 4)) shouldEqual  List(List(1,1,1), List(2,2,2), List(3,3), List(4,4))
  }





  //Problem 10
  "Applying the encode() function with the parameters List(\"a\", \"a\",\"a\", \"a\", \"b\", \"c\", \"c\", \"a\", \"a\", \"d\",\"e\", \"e\",\"e\", \"e\")" +
    "" should "return List((4,\"a\"), (1,\"b\"), (2,\"c\"), (2,\"a\"), (1,\"d\"), (4,\"e\"))" in {

    Lists.encode(List("a", "a","a", "a", "b", "c", "c", "a", "a", "d","e", "e","e", "e")) shouldEqual  List((4,"a"), (1,"b"), (2,"c"), (2,"a"), (1,"d"), (4,"e"))
  }

  "Applying the encode() function with the parameters List(1,2,3,3,3,3,8,8,8,5,5)" +
    "" should "return List((1,1), (1,2), (4,3), (3,8), (2,5))" in {

    Lists.encode(List(1,2,3,3,3,3,8,8,8,5,5)) shouldEqual  List((1,1), (1,2), (4,3), (3,8), (2,5))
  }



  //Problem 11
  "Applying the encodeModified2() function with the parameters List(1,2,3,3,3,3,8,8,8,5,5)" +
    "" should "return List(1, 2, (4,3), (3,8), (2,5))" in {

    Lists.encodeModified2(List(1,2,3,3,3,3,8,8,8,5,5)) shouldEqual  List(1, 2, (4,3), (3,8), (2,5))
  }


  "Applying the encodeModified2() function with the parameters List(1,2,3,5)" +
    "" should "return List(1, 2, 3, 5 )" in {

    Lists.encodeModified2(List(1,2,3,5)) shouldEqual  List(1, 2, 3, 5)
  }

  "Applying the encodeModified2() function with the parameters List(\"a\", \"a\",\"a\", \"a\", \"b\", \"c\", \"c\", \"a\", \"a\", \"d\",\"e\", \"e\",\"e\", \"e\")" +
    "" should "return List((4,\"a\"), \"b\", (2,\"c\"), (2,\"a\"), \"d\", (4,\"e\"))" in {

    Lists.encodeModified2(List("a", "a","a", "a", "b", "c", "c", "a", "a", "d","e", "e","e", "e")) shouldEqual  List((4,"a"), "b", (2,"c"), (2,"a"), "d", (4,"e"))
  }


  //Problem 12


  //Problem 13



  //Problem 14
  "Applying the dupli() function with the parameters List(\"a\",  \"b\", \"c\", \"c\", \"d\")" +
    "" should "return List(\"a\", \"a\", \"b\", \"b\", \"c\", \"c\", \"c\", \"c\", \"d\",\"d\")" in {

    Lists.dupli(List("a",  "b", "c", "c", "d")) shouldEqual  List("a", "a", "b", "b", "c", "c", "c", "c", "d","d")
  }

  "Applying the dupli() function with the parameters List(true,  true, true, false, false)" +
    "" should "return List(true, true, true, true, true, true, false, false, false,false)" in {

    Lists.dupli(List(true,  true, true, false, false)) shouldEqual  List(true, true, true, true, true, true, false, false, false,false)
  }

  "Applying the dupli() function with the parameters List(1,  2, 3, 3, 3)" +
    "" should "return List(1, 1, 2, 2, 3, 3, 3, 3, 3, 3)" in {

    Lists.dupli(List(1, 2, 3, 3, 3)) shouldEqual  List(1, 1, 2, 2, 3, 3, 3, 3, 3, 3)
  }



  //Problem 15
  "Applying the repli() function to the list List(\"a\", \"b\", \"c\") and n=3" +
    "" should "return List(\"a\", \"a\", \"a\",\"b\", \"b\", \"b\",\"c\", \"c\", \"c\")" in {

    Lists.repli(List("a", "b", "c"),3) shouldEqual  List("a", "a", "a","b", "b", "b","c", "c", "c")
  }
  "Applying the repli() function with the parameters List(true, true, false)" +
    "" should "return List(true, true, true, true, false, false)" in {

    Lists.repli(List(true, true, false),2) shouldEqual  List(true, true, true, true, false, false)
  }

  "Applying the repli() function with the parameters List(1) and n=2" +
    "" should "return List(true, true, true, true, false, false)" in {

    Lists.repli(List(true, true, false),2) shouldEqual  List(true, true, true, true, false, false)
  }



  //Problem 16
  "Applying the drop() function with the parameters List(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\", \"k\"), n = 3" +
    "" should "return  List(\"a\", \"b\", \"d\", \"e\", \"g\", \"h\", \"k\")" in {

    Lists.drop(List("a", "b", "c", "d", "e", "f", "g", "h", "i", "k"), 3) shouldEqual  List("a", "b", "d", "e", "g", "h", "k")
  }


  "Applying the drop() function with the parameters List(true, false, false, true,false, true), n = 1" +
    "" should "return List(true, false, false)" in {

    Lists.drop(List(true, false, false, true,false, true), 1) shouldEqual   List()
  }

  "Applying the drop() function with the parameters List(1,2,3,4,5,6), n = 6" +
    "" should "return List(1,2,3,4,5)" in {

    Lists.drop(List(1,2,3,4,5,6), 6) shouldEqual   List(1,2,3,4,5)
  }


  "Applying the drop() function with the parameters List(1,2,3,4,5,6), n = 7" +
    "" should "return List(1,2,3,4,5,6)" in {

    Lists.drop(List(1,2,3,4,5,6), 7) shouldEqual   List(1,2,3,4,5,6)
  }


  //Problem 17
  "Applying the split() function with the parameters List(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\", \"k\"), n = 3" +
    "" should "return List(List(\"a\", \"b\", \"c\"),List(\"d\", \"e\", \"f\", \"g\", \"h\", \"i\", \"k\"))" in {

    Lists.split(List("a", "b", "c", "d", "e", "f", "g", "h", "i", "k"), 3) shouldEqual  List(List("a", "b", "c"),List("d", "e", "f", "g", "h", "i", "k"))
  }

  "Applying the split() function with the parameters List(true,false), n = 5" +
    "" should "return List(true,false)" in {

    Lists.split(List(true,false), 5) shouldEqual  List(List(true,false))
  }

  "Applying the split() function with the parameters List(1,2,3), n = 3" +
    "" should "return List(List(1,2,3),List())" in {

    Lists.split(List(1,2,3), 3) shouldEqual  List(List(1,2,3),List())
  }

  "Applying the split() function with the parameters List(1,2,3), n = 1" +
    "" should "return List(List(1), List(2,3))" in {

    Lists.split(List(1,2,3), 1) shouldEqual  List(List(1),List(2,3))
  }



  //Problem 18

  "Applying the slice() function with the parameters List(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\", \"k\"), n1 = 3 n2 = 7" +
    "" should "return List( \"c\",\"d\", \"e\", \"f\", \"g\")" in {

    Lists.slice(List("a", "b", "c", "d", "e", "f", "g", "h", "i", "k"),3 , 7) shouldEqual  List( "c","d", "e", "f", "g")
  }

  "Applying the slice() function with the parameters List(1,2,3), n1 = 1 n2 = 4" +
    "" should "return List(1,2,3)" in {

    Lists.slice(List(1,2,3),1 , 4) shouldEqual  List(1,2,3)
  }

  "Applying the slice() function with the parameters List(true), n1 = 4 n2 = 5" +
    "" should "return List(true)" in {

    Lists.slice(List(true,false,false),4 , 5) shouldEqual  List()
  }

  "Applying the slice() function with the parameters List(true), n1 = 3 n2 = 3" +
    "" should "return List(true)" in {

    Lists.slice(List(true,false,false),3 , 3) shouldEqual  List(false)
  }




  //Problem 19



  //Problem 20
  "Applying the removeAt() function with the parameters List(\"a\", \"b\", \"c\", \"d\"), n = 2" +
    "" should "return (List(\"b\"),List(\"a\", \"c\", \"d\"))" in {

    Lists.removeAt(List("a", "b", "c", "d") , 2) shouldEqual  (List("b"),List("a", "c", "d"))
  }

  "Applying the removeAt() function with the parameters List(\"a\", \"b\", \"c\", \"d\"), n = 5" +
    "" should "return (List(),List(\"a\",\"b\", \"c\", \"d\"))" in {

    Lists.removeAt(List("a", "b", "c", "d") , 5) shouldEqual  (List(),List("a","b","c", "d"))
  }


  "Applying the removeAt() function with the parameters List(1,2,3,4,5,6), n = 6" +
    "" should "return (List(6),List(1,2,3,4,5))" in {

    Lists.removeAt(List(1,2,3,4,5,6) , 6) shouldEqual  (List(6),List(1,2,3,4,5))
  }



  //Problem 21
  "Applying the inserAt() function with the parameters \"Alfa\", List(\"a\", \"b\", \"c\", \"d\"), n = 2" +
    "" should "return List(\"a\",\"Alfa\",\"b\", \"c\", \"d\"))" in {

    Lists.insertAt("Alfa",List("a", "b", "c", "d"),2) shouldEqual  List("a","Alfa","b", "c", "d")
  }

  "Applying the inserAt() function with the parameters 1, List(1,2,3), n = 3" +
    "" should "return List(1,2,1,3)" in {

    Lists.insertAt(1,List(1,2,3),3) shouldEqual  List(1,2,1,3)
  }

  "Applying the inserAt() function with the parameters false, List(true,true), n = 1" +
    "" should "return List(false,true,true)" in {

    Lists.insertAt(false,List(true,true),1) shouldEqual  List(false,true,true)
  }



  //Problem 22

  "Applying the range() function with the parameters from = 4 to = 9" +
    "" should "return List(4,5,6,7,8,9)" in {

    Lists.range(4,9) shouldEqual  List(4,5,6,7,8,9)
  }

  "Applying the range() function with the parameters from = -1 to = 9" +
    "" should "return List(-1,0,1,2,3,4,5,6,7,8,9)" in {

    Lists.range(-1,9) shouldEqual  List(-1,0,1,2,3,4,5,6,7,8,9)
  }

  "Applying the range() function with the parameters from = 100 to = 110" +
    "" should "return List(100,101,102,103,104,105,106,107,108,109,110)" in {

    Lists.range(100,110) shouldEqual  List(100,101,102,103,104,105,106,107,108,109,110)
  }



  //Problem 23
  /*This exercise returns a random result, so  to test it it will return an error, but the implementation
  is fine.

*/

  "Applying the rndSelect() function with the parameters List(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\") n = 3" +
    "" should "return a List with random n elements of the given list" in {

    Lists.rndSelect(List("a", "b", "c", "d", "e", "f", "g", "h"),3) shouldEqual  List("g","d","c")
  }













}
