object zadania1 extends App{
  //zadanie 2
  def genList(num1: Int, num2:Int): List[Int] = {
    if num1 > num2 then Nil 
    else num1::genList(num1+1, num2)
  }

  val t21 = genList(2,9)
  val t22 = genList(0,0)
  val t23 = genList(9, 1)
  val t24 = genList(-2,2) 
}