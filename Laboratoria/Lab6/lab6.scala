
sealed trait instr
case class Push(c: Char) extends instr
case object Pop extends instr
case object Upper extends instr
case object Lower extends instr

object lab6 extends App {
  type Stack = List[Char]

  def eval(instrs: List[instr]): Stack = {
    def pop(st: Stack) = {
        st match
            case head :: next => (head, next)
            case Nil => throw new NoSuchElementException("brak elementow na stosie!")
        }
    def push(st: Stack, elem: Char) =  {  
        elem :: st      
        }

    def evaluate(instrs: List[instr], stack: Stack): Stack = {
        instrs match
            case Nil => stack
            case head :: next =>
                head match
                    case Push(c) => evaluate(next, push(stack, c))
                    case Pop => evaluate(next, pop(stack)._2)
                    case Upper => {
                        val (elem, st) = pop(stack) 
                        val upper = elem.toUpper
                        evaluate(next, push(st, upper))
                    }
                    case Lower => {
                        val (elem, st) = pop(stack) 
                        val lower = elem.toLower
                        evaluate(next, push(st, lower))
                    }
        }
        evaluate(instrs, List[Char]())
    }

    println("test: " + List(Push('a'), Push('b'), Upper, Pop, Push('c'), Upper).toString() )
    val test = eval(List(Push('a'), Push('b'), Upper, Pop, Push('c'), Upper))
    println(test.toString())

    println()
    println("test: " + List(Pop).toString() )
    try {
    val test1 = eval(List(Pop))
    } catch {
        case e: NoSuchElementException => e.printStackTrace()
    }

    println()
    println("test: " + List(Push('a'), Pop, Upper).toString() )
    println()
    try {
    val test2 = eval(List(Push('a'), Pop, Upper))
    } catch {
        case e: NoSuchElementException => e.printStackTrace()
    }
}
