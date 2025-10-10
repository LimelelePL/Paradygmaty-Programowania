(*Napisz funkcję rekurencyjną ostatni_element, która zwraca ostatni element listy.*)
let rec ostatniElement list =
    if list = [] then raise(Failure "pusta lista")
    else if List.tl list = [] then List.hd list
    else ostatniElement(List.tl list);;

let test = ostatniElement [1; 2; 3; 4] ;;
Printf.printf "%d\n" test;;

(*napisz funkcję, która rekurencyjnie oblicza sumę cyfr podanej liczby całkowitej*)

let rec sumaLiczbyCalkowitej liczba =
    if liczba = 0 then 0
    else liczba mod 10 + sumaLiczbyCalkowitej((liczba/10));;

let test2 = sumaLiczbyCalkowitej(129);;
Printf.printf "%d\n" test2;;

(*stwórz funkcje zastosujDwaRazy, która przyjmiuje jako argument funkcje f oraz wartość x i zwraca f(x)*)

let zastosujDwaRazy f x = f(f x);;
let podwoj x = 2*x;;
let test3 = zastosujDwaRazy podwoj 3;;
Printf.printf "%d\n" test3;;

(*Napisz funkcję, która przyjmuje krotkę zawierającą napis, 
liczbę całkowitą i liczbę zmiennoprzecinkową, a następnie zwraca nową krotkę zawierającą:
Długość napisu.
Liczbę całkowitą powiększoną o część całkowitą liczby zmiennoprzecinkowej.
*)

let przetworzKrotke (str, integer, float) = (String.length str, integer + int_of_float(float));;
let test4 = przetworzKrotke("kupa", 2, 2.5);;
let toString (a,b) = string_of_int a ^ " " ^ string_of_int b ;;
print_string (toString test4);;