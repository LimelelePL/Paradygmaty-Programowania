object cw1 extends App {
def flatten [A] (list : List[List[A]]) : List[A] = {
    if list.length == 0 then List[A]()
    else if list.tail.length == 0 then list.head
    else list.head ::: flatten (list.tail)
  }

    val flattened = flatten( List( List(1,2,3) , List(9, 2) ) )
    println(flattened.toString())
}
