object cw5 extends App { 

def isPalindrome [A] (list : List[A]) : Boolean = {
  list match {  
        case Nil => true
        case List(c) => true
        case a :: b  => (a == b.last) && isPalindrome(b.init)
        }
}

def listLength [A] (list: List[A] ) = {
  @annotation.tailrec
  def iter [A] (xs : List[A], accum : Int ): Int= 
     xs match {
      case Nil => accum
      case _ => iter(xs.tail, accum + 1)
    }
  iter(list, 0) 
}

val l = listLength(List(1,2,3))
 /* 5. Zdefiniuj funkcję palindrome : 'a list -> bool sprawdzającą, czy dana lista jest
palindromem, tj. równa się sobie samej przy odwróconej kolejności elementów,
np. palindrome ['a'; 'l'; 'a'] zwraca true.
Scala palindrome: [A] (xs: List[A]) Boolean

6. Zdefiniuj swoją funkcję listLength : 'a list -> int, obliczającą długość dowolnej listy
(oczywiście bez użycia standardowej funkcji List.length).
Scala listLength: [A](xs: List[A])Int */

}
