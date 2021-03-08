package example

import scala.annotation.tailrec
import scala.util.Random

object Lists extends App {

  //Taller 6 Lists en Scala
  //Exercise 1.

  def subs [A] (lst:List[A]) : List[List[A]] = lst match {
    case head :: tail => subs(tail) ::: subs(tail).map(head :: _)
    case Nil => List(Nil)
  }



  //------------------------------------------------------------------------------------------------------------------



  //Excersie 2.
  def barajar [A] (a:A, lst:List[A]) : List[List[A]] = lst match {
    case x :: xs => (a :: (x :: xs)) :: (barajar(a,xs)).map(x::_)
    case Nil => List(List(a))
  }

  println(barajar(1,List(2,3)))

  def perms [A] (lst:List[A]): List[List[A]] = lst match {
    case x :: xs => (perms(xs)).flatMap(barajar(x,_))
    case Nil => List(Nil)
  }





  //99 QUESTIONS  https://wiki.haskell.org/99_questions/1_to_10



  //Problem 1
  //Find the last element of a list. Lists with one element and more
  def lastElementList[A](lst: List[A]): A = lst match {
    case head :: Nil => head
    case head :: tail => lastElementList(tail)

  }

  println("PROBLEM 1 ==> " + lastElementList(List("x","y","z")))


  //--------------------------------------------------------------------------------------------------------------


  //Problem 2
  //Lists of two or more elements
  //Find the last but one element of a list.
  def lastButOneElement[A](lst: List[A]): A = lst match {
    case x :: y :: Nil => x
    case head :: tail => lastButOneElement(tail)
  }

  println("PROBLEM 2 ==> " + lastButOneElement(List(1, 2, 3, 4)))


  //--------------------------------------------------------------------------------------------------------------

  //Problem Bonus
  //Find the last and the last but one elements of a list.
  def lastAndLastButOne[A](lst: List[A]): List[A] = lst match {
    case x :: y :: Nil => List(x,y)
    case head :: tail => lastAndLastButOne(tail)
  }

  println("PROBLEM BONUS: " + lastAndLastButOne(List(1, 2, 3, 4, 5)))


  //--------------------------------------------------------------------------------------------------------------

  //Problem 3
  //Find the K'th element of a list.
  def kElement[A](lst: List[A], pos: Int): A = (lst, pos) match {
    case (head :: tail, 1) => head
    case (head :: tail, n) => kElement(tail, pos - 1)
  }

  println("PROBLEM 3 => " + kElement(List(1, 2, 3, 4, 5), 3))


  //--------------------------------------------------------------------------------------------------------------


  //Problem 4
  //Find the number of elements of a list.
  def myLengthFL[A](lst: List[A]): Int = {
    def aux[A](lst: List[A], acc: Int): Int = lst match {
      case Nil => acc
      case head :: tail => aux(tail, acc + 1)
    }

    aux(lst, 0)
  }

  println("PROBLEM 4 => " + myLengthFL(List(1, 2, 3)))


  //--------------------------------------------------------------------------------------------------------------

  //Problem 5
  //Reverse a list.
  def myReverse[A](lst: List[A]): List[A] = {
    def aux[A](lst: List[A], lst2: List[A]): List[A] = lst match {
      case Nil => lst2
      case head :: tail => aux(tail, (head :: lst2))
    }

    aux(lst, Nil)
  }

  println("PROBLEM 5 => " + myReverse(List(1, 2, 3, 4, 5)))



  //--------------------------------------------------------------------------------------------------------------

  //Problem 6
  //Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
  def pal[A](lst: List[A]): Boolean = {
    def aux(lst: List[A], lst2: List[A]): List[A] = lst match {
      case Nil => lst2
      case head :: tail => aux(tail, head :: lst2)
    }

    aux(lst, Nil) == lst
  }

  def pal2[A](lst: List[A]): Boolean = myReverse(lst) == lst


  println("PROBLEM 6 => " + pal(List(1, 2, 2, 3, 4)))
  println("PROBLEM 6 => " + pal(List(1, 2, 2, 3, 2, 2, 1)))


  //--------------------------------------------------------------------------------------------------------------

  sealed trait NestedLista[+A]

  case class Elemento[A](a: A) extends NestedLista[A]

  case class Lista[A](lst: List[NestedLista[A]]) extends NestedLista[A]

  val nl1 = Lista(List(Elemento(1), Lista(List(Elemento(2))), Elemento(3)))

  //Problem 7.


  /*
  FLATMAP
  def flatten [A] (lst: List[NestedLista[A]]): List[A] = {
    def auxFlatten
  }

   */


  /*
  flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
   */


  //--------------------------------------------------------------------------------------------------------------

  //Problem 8.
  // Eliminate consecutive duplicates of list elements.
  def compress[A](lst: List[A]): List[A] = {
    def aux[A](lst: List[A], lstAcc: List[A]): List[A] = (lst, lstAcc) match {
      case (h :: t, Nil) => aux(t, h :: lstAcc)
      case (h :: t, h2 :: t2) => if (h == h2) aux(t, lstAcc) else aux(t, h :: lstAcc)
      case (Nil, _) => lstAcc.reverse
    }

    aux(lst, Nil)
  }

  println("PROBLEM 8 => " + compress(List(1, 1, 1, 2, 2, 2, 3, 3, 4, 4)))


  //--------------------------------------------------------------------------------------------------------------


  //Problem 9
  //Pack consecutive duplicates of list elements into sublists.
  def pack[A](lst: List[A]): List[List[A]] = {
    def aux[A](lst: List[A], lstAcc: List[List[A]], lst1: List[A]): List[List[A]] = lst match {
      case x :: y :: xs => if (x == y) aux(y :: xs, lstAcc, x :: lst1) else aux(y :: xs, lstAcc ::: List(x :: lst1), Nil)
      case y :: Nil => lstAcc ::: List(y :: lst1)
    }

    aux(lst, List(), Nil)
  }

  println("PROBLEM 9 => " + pack(List(1, 1, 1, 2, 2, 2, 3, 4, 4)))


  //--------------------------------------------------------------------------------------------------------------


  //Problem 10
  //Run-length encoding of a list.Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E
  def encode[A](lst: List[A]): List[(Int, A)] = {
    def aux[A](lst: List[A], lstAcc: List[(Int, A)], acc: Int): List[(Int, A)] = lst match {
      case x :: y :: xs => if (x == y) aux(y :: xs, lstAcc, acc + 1) else aux(y :: xs, lstAcc ::: List((acc, x)), 1)
      case y :: Nil => lstAcc ::: List((acc, y))
    }

    aux(lst, List(), 1)
  }

  println("PROBLEM 10 => " + encode(List(1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4)))
  println("PROBLEM 10 => " + encode(List("a", "a", "b", "b", "b", "b")))


  //Problem 11
  //RETURNS ONLY STRINGS
  def encodeModified[A](lst: List[A]): List[(String)] = {
    def aux[A](lst: List[A], lstAcc: List[(String)], acc: Int): List[(String)] = lst match {
      case x :: y :: xs => if (x == y) aux(y :: xs, lstAcc, acc + 1)
                            else aux(y :: xs, lstAcc ::: List(if (acc > 1) s"Multiple $acc '$x'" else s"Single '$x'"), 1)
      case y :: Nil => lstAcc ::: List(if (acc > 1) s"Multiple $acc '$y'" else s"Single '$y'")
    }

    aux(lst, List(), 1)
  }

  println("PROBLEM 11 => " + encodeModified(List(1, 1, 1, 2, 4, 3, 3, 3)))
  println("PROBLEM 11 => " + encodeModified(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e")))


  //RETURNS TUPLES AND STRINGS
  def encodeModified2 [A] (lst: List[A]): List[Any] = {
    def aux[A](lst: List[A], lstAcc: List[Any], acc: Int): List[Any] = lst match {
      case x :: y :: xs => if (x == y) aux(y :: xs, lstAcc, acc + 1)
                            else aux(y :: xs, lstAcc ::: List(if (acc > 1) (acc,x) else (x)), 1)
      case y :: Nil => lstAcc ::: List(if (acc > 1) (acc,y) else (y))
    }

    aux(lst, List(), 1)
  }


  println("PROBLEM 11 => " + encodeModified2(List(1, 1, 1, 2, 4, 3, 3, 3)))




  //--------------------------------------------------------------------------------------------------------------


  //Problem 12


  //--------------------------------------------------------------------------------------------------------------


  //Problem 13


  //--------------------------------------------------------------------------------------------------------------


  //Problem 14.
  //Duplicate the elements of a list.
  def dupli[A](lst: List[A]): List[A] = {
    @tailrec
    def aux[A](lst: List[A], lstAcc: List[A]): List[A] = lst match {
      case x :: xs => aux(xs, lstAcc ::: List(x) ::: List(x))
      case Nil => lstAcc
    }

    aux(lst, Nil)
  }

  println("PROBLEM 14 => " + dupli(List(1, 2, 3, 4, 5)))
  println("PROBLEM 14 => " + dupli(List("A", "B", "C", "C", "D")))


  //--------------------------------------------------------------------------------------------------------------


  //Problem 15.
  //Replicate the elements of a list a given number of times.
  def repli[A](lst: List[A], n: Int): List[A] = {
    def aux[A](lst: List[A], n2: Int, listAcc: List[A]): List[A] = lst match {
      case x :: xs => if (n != n2) aux(lst, n2 + 1, listAcc ::: List(x)) else aux(xs, 0, listAcc)
      case Nil => listAcc
    }

    aux(lst, 0, Nil)
  }

  println("PROBLEM 15 => " + repli(List(1, 2, 3, 4), 5))
  println("PROBLEM 15 => " + repli(List("a", "b", "c"), 3))


  //--------------------------------------------------------------------------------------------------------------


  //Problem 16.
  //Drop every N'th element from a list.
  def drop[A](lst: List[A], n: Int): List[A] = {
    def aux[A](lst: List[A], n2: Int, lstAcc: List[A]): List[A] = lst match {
      case x :: xs => if (n == n2) aux(xs, 1, lstAcc) else aux(xs, n2 + 1, lstAcc ::: List(x))
      case Nil => lstAcc
    }

    aux(lst, 1, Nil)
  }

  //println(drop(List(1,2,3,4),3))
  println("PROBLEM 16 => " + drop(List("a", "b", "c", "d", "e", "f", "g", "h", "i", "k"), 3))
  println("PROBLEM 16 => " + drop(List("a", "b", "c", "d", "e", "f", "g", "h", "i", "k"), 8))


  //--------------------------------------------------------------------------------------------------------------


  //Problem 17
  //Split a list into two parts
  def split[A](lst: List[A], n: Int): List[List[A]] = {
    @tailrec
    def aux[A](lst: List[A], n2: Int, lstAcc: List[A], lstlstAcc: List[List[A]]): List[List[A]] = lst match {
      case x :: xs => if (n == n2) aux(Nil, n2, xs, lstlstAcc ::: List(lstAcc ::: List(x)))
      else aux(xs, n2 + 1, lstAcc ::: List(x), lstlstAcc)
      case Nil => lstlstAcc ::: List(lstAcc)


    }

    aux(lst, 1, Nil, List())
  }

  println("PROBLEM 17 => " + split(List("a", "b", "c", "d", "e", "f", "g", "h", "i", "k"), 3))
  println("PROBLEM 17 => " + split(List("a", "b", "c", "d", "e", "f", "g", "h", "i", "k"), 8))


  //--------------------------------------------------------------------------------------------------------------


  //Problem 18
  //Extract a slice from a list.
  def slice[A](lst: List[A], n1: Int, n2: Int): List[A] = {
    def aux[A](lst: List[A], lstAcc: List[A], nCounter: Int): List[A] = lst match {
      case x :: xs => if (nCounter < n1 || nCounter > n2) aux(xs, lstAcc, nCounter + 1)
      else aux(xs, lstAcc ::: List(x), nCounter + 1)
      case Nil => lstAcc
    }

    aux(lst, Nil, 1)
  }

  println("PROBLEM 18 => " + slice(List("a", "b", "c", "d", "e", "f", "g", "h", "i", "k"), 3, 7))
  println("PROBLEM 18 => " + slice(List("a", "b", "c", "d", "e", "f", "g", "h", "i", "k"), 2, 8))


  //--------------------------------------------------------------------------------------------------------------


  //Problem 19
  //PENDIENTE EL CASO NEGATIVO
  def rotate[A](lst: List[A], n: Int): List[A] = {
    @tailrec
    def aux(n: Int, lst: List[A], lstAcc: List[A], lstAcc2: List[A], n2: Int): List[A] = (lst, n compare 1) match {
      case (x :: xs, 1) => if (n2 < n) aux(n, xs, lstAcc ::: List(x), lstAcc2, n2 + 1)
      else aux(n, Nil, lstAcc ::: List(x), lstAcc2 ::: xs, n2 + 1)
      case (x :: xs, -1) => rotate(lst.reverse, n * (-1))
      case (Nil, 1) => lstAcc2 ::: lstAcc
      case (Nil, -1) => lstAcc.reverse ::: lstAcc2.reverse
    }

    aux(n, lst, Nil, Nil, 1)
  }

  println("PROBLEM 19 => " + rotate(List("a", "b", "c", "d", "e", "f", "g", "h"), 3))
  println("PROBLEM 19 => " + rotate(List("a", "b", "c", "d", "e", "f", "g", "h"), -2))
  //FALTAAAA


  //--------------------------------------------------------------------------------------------------------------


  //Problem 20
  //Remove the K'th element from a list.
  def removeAt[A](lst: List[A], n: Int): (List[A], List[A]) = {
    def aux(lst: List[A], lst1: List[A], lst2: List[A], n2: Int): (List[A], List[A]) = lst match {
      case x :: xs => if (n == n2) aux(xs, x :: lst1, lst2, n2 + 1)
      else aux(xs, lst1, lst2 ::: List(x), n2 + 1)
      case Nil => (lst1, lst2)
    }

    aux(lst, Nil, Nil, 1)
  }

  println("PROBLEM 20 => " + removeAt(List("a", "b", "c", "d"), 2))
  println("PROBLEM 20 => " + removeAt(List("a", "b", "c", "d"), 1))
  println("PROBLEM 20 => " + removeAt(List("a", "b", "c", "d"), 4))
  println("PROBLEM 20 => " + removeAt(List("a", "b", "c", "d"), 5))


  //--------------------------------------------------------------------------------------------------------------


  //Problem 21
  //Insert an element at a given position into a list.
  def insertAt[A](elemToInsert: A, lst: List[A], position: Int): List[A] = {
    def aux[A](elemToInsert: A, lst: List[A], lstAcc: List[A], n2: Int): List[A] = lst match {
      case x :: xs => if (position == n2) aux(elemToInsert, xs, lstAcc ::: List(elemToInsert) ::: List(x), n2 + 1)
      else aux(elemToInsert, xs, lstAcc ::: List(x), n2 + 1)
      case Nil => lstAcc
    }

    aux(elemToInsert, lst, Nil, 1)
  }

  println("PROBLEM 21 => " + insertAt("X", List("a", "b", "c", "d"), 2))
  println("PROBLEM 21 => " + insertAt("X", List("a", "b", "c", "d"), 4))
  println("PROBLEM 21 => " + insertAt("X", List("a", "b", "c", "d"), 1))
  println("PROBLEM 21 => " + insertAt("alfa", List("a", "b", "c", "d"), 2))


  //--------------------------------------------------------------------------------------------------------------


  //Problem 22
  //Create a list containing all integers within a given range.
  def range[A](from: Int, to: Int): List[Int] = {
    def aux[A](from: Int, to: Int, lstAcc: List[Int]): List[Int] = (from compare to) match {
      case  0 => lstAcc ::: List(to)
      case -1 => aux(from+1,to, lstAcc ::: List(from))

    }
    aux(from,to,Nil)
  }
  println("PROBLEM 22 => " + range(4,9))
  println("PROBLEM 22 => " + range(1,20))



  //--------------------------------------------------------------------------------------------------------------



  //Problem 23
  //Extract a given number of randomly selected elements from a list.
  def rndSelect[A](lst: List[A], n: Int): List[A] = {
    def aux(lst: List[A], lstAcc: List[A], n: Int): List[A] = n match {
      case n => aux(lst, (kElement(lst,Random.nextInt(lst.length)) :: lstAcc), n - 1)
      case 0 => lstAcc
    }
    aux(lst, Nil, n)
  }

  println("PROBLEM 23 => " + rndSelect(List("a", "b", "c", "d", "e", "f", "g", "h"),3))



  //--------------------------------------------------------------------------------------------------------------



  //Problem 24
  //Draw n different random numbers from a list of integers with an specified range from 1 to max
  def diffSelect[A](n: Int, max: Int): List[Int] = rndSelect(range(1, max), n)

  println("PROBLEM 24 => " + diffSelect(6,49))



  //--------------------------------------------------------------------------------------------------------------



  //Problem 25


  def rndmPermu[A](lst: List[A]): List[A] = rndSelect(perms(lst), 1).flatten

  println("PROBLEM 25 => " + rndmPermu(List("a", "b", "c", "d", "e", "f") ))



  //--------------------------------------------------------------------------------------------------------------



  //Problem 26







}