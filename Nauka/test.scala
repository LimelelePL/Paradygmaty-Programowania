
def count [A] (f: A => Boolean, list: List[A]) : Int = {
    list match
        case head :: next => if f (head) then 1 + count(f, next) else count(f, next) 
        case Nil => 0
} 

def squaresOfEvens (xs: List[Int]) : List[Int] = {
    (xs.filter(x => x % 2 == 0) ).map(x => x*x)
}
val squares =  squaresOfEvens(List(1,2,3,4,5));;