object lista1 extends App {
  
  /* Napisać funkcję fiddle4 zmieniającą elementy podanej krotki czteroelementowej w
następujący sposób: (1.3, 2.0, 3.1, 4.2) -> (4.2, 1.3, 3.1 - 2.0). (OCaml i Scala) (10pkt.) */

def fiddle4 [A] (krotka : (A, A ,A,A)) :(A,A,A,A) = {
    val (x,y,z,w) = krotka
    (w,y,z,x)
}

/* Napisać funkcję hits przyjmującą dwie listy i zliczającą na ilu pozycjach są one równe. (OCaml
i Scala) (10pkt.) */

def hits [A] (l1 : List[A], l2 : List[A]) : Int = {
    if l1.isEmpty || l2.isEmpty then 0
    else if l1.head == l2.head then 1 + hits(l1.tail , l2.tail)
    else hits(l1.tail, l2.tail)
    }

println(hits ( List(1,2,3), List(1,2,4) ) )

/*3) Napisać funkcję insert przyjmującą listę, nowy element oraz pozycję, na którą ma on być
wstawiony i zwracającą nową listę zawierającą wstawiany element. Jeśli pozycja jest poza
zakresem, element należy wstawić na odpowiednim końcu listy. (OCaml i Scala) (10pkt.)
4) Napisać funkcję militaryMinutes przyjmującą trójkę wartości: dwie liczby reprezentujące
godzinę i minuty w systemie 12-o godzinnym oraz łańcuch znaków zawierający porę dnia –
„AM” / „PM”, i zwracającą napis zawierający tę godzinę w systemie 24-o godzinnym postaci:
„HH : MM”. W razie błędu rzucić wyjątek z odpowiednim komunikatem tekstowym. (OCaml i
Scala) (20pkt.)
*/
}


