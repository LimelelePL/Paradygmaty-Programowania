object cw4 extends App{
    def sqrList (list : List[Int]) : List[Int] = { 
    if list.isEmpty then Nil
    else (list.head * list.head) :: sqrList(list.tail)
    }
    
    val square = sqrList(List(2,3,4))
    print (square.toString())
}
