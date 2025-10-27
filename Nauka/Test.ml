
let apply f x = f x;;
let t1 = apply (fun x-> 2*x) 2;;

let applyTwice f x = f (apply f x);;
let t2 = applyTwice (fun x -> 2*x) 2 ;;

let func1 x = 2*x;;
let func2 x = 4*x;;
let compose f g = fun x -> f (g x)

let rec my_filter pred xs = 
    match xs with
    | h::t  when pred h  -> h :: my_filter pred t 
    | h::t ->  my_filter pred t
    | [] -> [] ;;

let t3 = my_filter (fun x -> x = 2) [2;2;5;5;3];;

let rec my_fold_left f acc xs = 
    match xs with
    | h::t -> let acc' = f acc h in my_fold_left f acc' t 
    | [] -> acc ;;

let t4 =  my_fold_left (fun x acc -> acc + x ) 0 [1;2;3;4;5];;

(*
🔥 Poziom 3 — zabawa z wyższym rzędem
7️⃣ Zrób funkcję, która zwraca inną funkcję
(* addN : int -> (int -> int)
   addN n zwraca funkcję, która dodaje n do swojego argumentu. *)

let addN n = ...


✅ Test:

let add5 = addN 5;;
add5 10;;  (* wynik: 15 *)
(addN 2) 7;;  (* wynik: 9 *)

8️⃣ Funkcja, która zwraca odwrotność predykatu
(* negate : ('a -> bool) -> 'a -> bool *)
let negate p = ...


✅ Test:

let isEven x = x mod 2 = 0;;
let isOdd = negate isEven;;
isOdd 3;;  (* true *)
isOdd 4;;  (* false *)

9️⃣ Funkcja, która zwraca funkcję warunkową
(* choose : bool -> ('a -> 'a -> 'a)
   jeśli true → zwróć funkcję wybierającą pierwszy argument
   jeśli false → zwróć funkcję wybierającą drugi argument *)

let choose cond = ...


✅ Test:

(choose true) 10 20;;   (* 10 *)
(choose false) 10 20;;  (* 20 *)

🧩 Poziom 4 — małe kombinacje
🔟 Napisz map używając fold_right
(* map_v2 : ('a -> 'b) -> 'a list -> 'b list *)
let map_v2 f xs =
  List.fold_right (fun x acc -> (f x) :: acc) xs [];;
*)


(*🔥 Poziom 3 — zabawa z wyższym rzędem
7️⃣ Zrób funkcję, która zwraca inną funkcję
(* addN : int -> (int -> int)
   addN n zwraca funkcję, która dodaje n do swojego argumentu. *)

let addN n = ...


✅ Test:

let add5 = addN 5;;
add5 10;;  (* wynik: 15 *)
(addN 2) 7;;  (* wynik: 9 *) *)

let addN n = fun x -> x + n;;
let add5 = addN 5;;
let test5 = add5 9;;

(*8️ Funkcja, która zwraca odwrotność predykatu
(* negate : ('a -> bool) -> 'a -> bool *)
let negate p = ...


 Test:

let isEven x = x mod 2 = 0;;
let isOdd = negate isEven;;
isOdd 3;;  (* true *)
isOdd 4;;  (* false *)
*)

let isEven x = x mod 2 = 0;;
let negate p = fun x -> if p x then false else true;; 
let isOdd =  negate isEven;;
let x = isOdd 3;;

(*
9️⃣ Funkcja, która zwraca funkcję warunkową
(* choose : bool -> ('a -> 'a -> 'a)
   jeśli true → zwróć funkcję wybierającą pierwszy argument
   jeśli false → zwróć funkcję wybierającą drugi argument *)

let choose cond = ...


✅ Test:

(choose true) 10 20;;   (* 10 *)
(choose false) 10 20;;  (* 20 *)
*)

let choose cond = if cond then fun a b -> a else fun a b -> b;;

(choose true) 10 20;;
(choose false) 10 20;;

(*
🧩 Poziom 4 — małe kombinacje
🔟 Napisz map używając fold_right
(* map_v2 : ('a -> 'b) -> 'a list -> 'b list *)*)

let map_v2 f xs = List.fold_right (fun x acc -> (f x) :: acc) xs [];;

