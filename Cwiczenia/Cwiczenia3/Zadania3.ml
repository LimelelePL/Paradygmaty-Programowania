(*zadanie 2*)

let curry f x y z = f (x,y,z);;
let curry = function f-> function x -> function y -> function z -> f(x,y,z);;
let uncurry f (x,y,z) = f x y z;;
let uncurry = function f -> function (x,y,z) -> f x y z 

(* zadanie 3*)

(* List.fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
          List.fold_left f init xs 
*  a- akumulator 
   b- kolejny element listy
   a- nowy akumulator
*)
let sumProd xs = List.fold_left ( fun (sum , mult) x -> (sum+x, mult*x)) (0,1) xs;; 
let s = sumProd [1;2;3];;

(*zadanie 5 - nie umiem*)

let rec insert comp elem list = 
  match list with
   | [] -> [elem]
   | head :: tail -> if comp elem head then elem :: list else head :: insert comp elem tail;;

let rec sort comp list =
  match list with
   | [] -> []
   | head :: tail -> insert comp head (sort comp tail);;

let s = sort (fun a b -> a <= b) [7;5;3;1;10;23];;
