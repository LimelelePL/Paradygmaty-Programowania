let rec applyNTimes f x n = 
    if n = 0 then x 
    else applyNTimes f (f x) (n-1);;

let z = applyNTimes (fun x-> x + x) 5 3;;
         

let map f xs = 
   List.fold_right (fun x acc -> (f x) :: acc) xs [];;


let t = List.fold_left (fun acc x -> acc ^ x) " " ["a";"b";"c";"d"];;

(*3️Napisz funkcję count(pred, xs)

która zlicza, ile elementów listy spełnia predykat pred.
Przykład:
count(x => x mod 2 == 0, [1;2;3;4;6]) → 3.*)
let rec count pred xs = 
    match xs with 
    | head :: tail when pred head-> 1 + count pred tail
    | head :: tail -> count pred tail
    | [] -> 0;;

let c = count (fun x -> x mod 2 = 0) [1;2;3;4;6];;

(* Używając tylko map i filter, utwórz funkcję squaresOfEvens(xs)

która zwraca kwadraty wszystkich liczb parzystych z listy xs.
Przykład:
squaresOfEvens([1;2;3;4;5]) → [4;16].*)

let squaresOfEvens xs =
    List.map (fun x -> x*x) (List.filter (fun x -> x mod 2 = 0) xs);;

let k = squaresOfEvens[1;2;3;4;5];;

(*Napisz funkcję concatLists(xss)

która spłaszcza listę list (czyli [[1;2];[3];[4;5]] → [1;2;3;4;5])
– ale nie używaj operatora @ ani List.concat.
Wykorzystaj fold_left. *)

let rec concatLists xss = 
    match xss with 
    | head :: tail -> List.fold_left (fun acc x -> x :: acc) [] head 
    |  