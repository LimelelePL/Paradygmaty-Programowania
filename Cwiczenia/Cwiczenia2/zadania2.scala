import scala.annotation.tailrec
import scala.compiletime.ops.double
object zadania2 extends App {
 /* zad 1
 * Scala nie optymalizuje funkcji wzajemnie rekurencyjnych, dlatego mamy głebokość stosu równą 4
evenR(3)
→ oddR(2)
→ evenR(1)
→ oddR(0)
→ false
 * Natomiast Ocaml rozpoznaje funkcje wzajemnie rekurencyjne i optymalizuje je do rekurencji ogonowej, dlatego
 * kompilator nie musi pamiętać poprzednich wywołań funkcji i stos ma głębokość 1
 * 
 */

  //zad2
  def normalFibonacci(n: Int) : Int = {
    if n==0 || n==1 then n
    else normalFibonacci(n-1) + normalFibonacci(n-2)
  }

  def tailFibonnaci(n:Int) : Int = {
    @annotation.tailrec
    def iter(n:Int, a:Int, b:Int) : Int = {
        if n == 0 then a
        else if n==1 then b
        else iter(n-1, b, a+b)
    }
    iter (n, 0, 1)
  }

  val t = tailFibonnaci(6);
  println(t)
  //zad 3

  def root3(a:Double):Double = {

    val ep : Double = 1.0e-15
    val x0 : Double = if math.abs(a)>1 then a/3 else a
    val accuracy: Double => Boolean = (xi : Double) => Math.abs(xi * xi * xi - a) <= ep * Math.abs(a)
    val res: Double => Double = (xi : Double ) => xi + (a/(xi*xi) - xi )/3.0

    @annotation.tailrec
    def iter(result : Double):Double ={ 
        if accuracy(result) then result
        else iter(res(result))
    }
    iter(x0)
  }

    val sqr = root3(3.0)
    println(sqr)

//zad 5
def initSegment[A](xs: List[A], ys: List[A]): Boolean = {
    if xs.isEmpty then true;
    (xs,ys) match {
     case (Nil, _) => true
     case (_, Nil) => false
     case (h1::t1, h2::t2) => if h1==h2 then initSegment(t1,t2) else false
    } 
}



//zad 6
def replaceNth[A](xs: List[A], n: Int, x: A): List[A] = {
    (xs, n) match {
        case (Nil, _) => Nil
        case (h1::t1, 0) => h1::t1
        case (h1::t1, _) => h1::replaceNth(t1, n-1, x)
    }
}

def replaceNthTail[A](xs: List[A], n: Int, x: A): List[A] = {
    def iter(xs:List[A], result: List[A], n:Int ) : List[A] =
    xs match {
        case (Nil) => result.reverse
        case (h1::t1) => if n!=0 then iter(t1, h1::result , n-1) else iter(t1, x :: result, n-1)
    }
    iter(xs, Nil, n)
}

val replaced = replaceNthTail(List(1,2,3,4), 2, 10);
print(replaced.toString())

//wspoldzieli ogon ta lista 

}