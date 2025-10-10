package Nauka

import scala.annotation.tailrec

object test extends App {
  @tailrec
  private def ostatniElement [A](list : List[A]) : A = {
    if list.isEmpty then throw new IllegalStateException("pusta lista")
    else if list.tail.isEmpty then list.head
    else ostatniElement(list.tail)
  }
  private val lastElem = ostatniElement(List(1, 2, 3))
  println(lastElem)

  private def sumOfNumbers (number : Int) : Int = {
    if number == 0 then 0
    else number % 10 + sumOfNumbers(number/10)
  }

  val sum = sumOfNumbers(129)
  println(sum)

  def zastosujDwaRazy [A] (function: A => A, x: A ): A = {
    function(function(x))
  }
  def podwoj (liczba: Int): Int = 2*liczba
  println(zastosujDwaRazy(podwoj, 2))
  def przetworzKrotkeMetoda (tuple:(String, Int, Float)): (Int, Float) ={
    (tuple._1.length, tuple._2 + tuple._3%10)
  }
  val przetworzKrotke: (String, Int, Float) => (Int, Float) = (a, b, c) => (a.length, b + c % 10)
}
