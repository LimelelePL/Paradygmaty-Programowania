
object zadania3 extends App{
  /* 2. Zdefiniuj funkcje a) curry3 i b) uncurry3, przeprowadzające konwersję między zwiniętymi i rozwiniętymi
postaciami funkcji od trzech argumentów. Podaj (i uzasadnij!) ich typy.
Dla każdej funkcji napisz dwie wersje: z lukrem syntaktycznym i bez lukru syntaktycznego.
 */

def curry3[A, B, C, D](f: ((A, B, C)) => D): A => B => C => D =
  (x: A) => (y: B) => (z: C) => f((x, y, z))

def curry3Lukier[A,B,C,D](f: ((A,B,C)) => D) : A=>B=>C=>D = 
    (x:A) => {
        (y:B) => {
            (z:C) => {
                f(x,y,z)
            }
        }
    }

 def sumProdList(xs : List[Int]) : (Int,Int) = 
    xs.foldLeft((0,1)) {
        case ((sum, mult), x) => (sum+x, mult*x)
    } 

println(sumProdList(List()).toString())

}
